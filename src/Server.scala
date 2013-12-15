import java.net.ServerSocket
import scala.collection.mutable.Map
import scala.actors.Actor.{actor, loop, react}

class UserData(var password: String, var balance: Double, val assets: Map[String, Int])
//Assets: stock ticker -> amount held

object OrderType extends Enumeration {
  type OrderType = Value
  val BuyOrder, SellOrder = Value
}

//TODO: discrete tick size (problems with FP precicion otherwise)
import OrderType._
class Order(val id: Int, val username: String, val ticker: String, var amount: Int, val price: Double, val orderType: OrderType)

class Server (port: Int) {
  val handlersMap = Map[Int, ClientHandler]() //connection ID -> handler thread
  val usernamesMap = Map[Int, String]()       //connection ID -> username
  val connectionsMap = Map[String, Int]()     //username -> connection ID
  val users = Map[String, UserData]()         //username -> password, balance, assets
  var lastConnectionId = 0                    //Each connection has a unique ID
  var lastOrderId = 0                         //Each order has a unique ID
  val orderBook = Map[Int, Order]()           //order ID -> order data

  //Add some test users
  users += ("test" -> new UserData("test", 100.0, Map()))
  users += ("test2" -> new UserData("test", 0.0, Map("AAPL" -> 10)))

  def tryLogin(connectionId: Int, username: String, password: String) =
    if (
      (usernamesMap contains connectionId) ||
        (connectionsMap contains username) ||
        !(users contains username) ||
        (users(username).password != password)) LoginFailure()
    else {
      usernamesMap += (connectionId -> username)
      connectionsMap += (username -> connectionId)
      LoginSuccess()
    }

  def logout(connectionId: Int) {
    if (usernamesMap contains connectionId) {
      connectionsMap.remove(usernamesMap(connectionId))
      usernamesMap.remove(connectionId)
    }
  }

  def matchOrders() {
    //Organize orders per-commodity
    val buys, sells = Map[String, Set[Order]]()

    for (order <- orderBook.values) {
      if (order.orderType == BuyOrder) {
        if (!(buys contains order.ticker)) buys += (order.ticker -> Set())
        buys(order.ticker) += order
      } else {
        if (!(sells contains order.ticker)) sells += (order.ticker -> Set())
        sells(order.ticker) += order
      }
    }

    for (ticker <- buys.keys if sells contains ticker) {
      var bids = buys(ticker).toList.sortBy(o => o.price).reverse
      var asks = sells(ticker).toList.sortBy(o => o.price).reverse

      while (bids.nonEmpty && asks.nonEmpty && bids.head.price >= asks.head.price) {
        val bid = bids.head
        val ask = asks.head
        val bidUser = users(bid.username)
        val askUser = users(ask.username)

        val amount = bid.amount min ask.amount
        val price = (bid.price + ask.price) / 2.0

        //Update the users' balances (TODO: freeze balances when placing an order?
        bidUser.balance -= price * amount
        askUser.balance += price * amount

        //Update the users' assets (TODO: freeze assets when placing an order?
        if (!(bidUser.assets contains bid.ticker)) bidUser.assets(bid.ticker) = 0
        bidUser.assets(bid.ticker) += amount
        askUser.assets(bid.ticker) -= amount
        if (askUser.assets(bid.ticker) == 0) askUser.assets.remove(bid.ticker)

        //Notify the users about the execution (partial or not) if they are online
        if (connectionsMap contains bid.username)
          handlersMap(connectionsMap(bid.username)) ! Executed(bid.id, amount, price)
        if (connectionsMap contains ask.username)
          handlersMap(connectionsMap(ask.username)) ! Executed(ask.id, amount, price)

        //Update the order amounts (order book is updated automatically)
        bid.amount -= amount
        ask.amount -= amount

        //If an order has been completely filled, remove it from the order book.
        if (bid.amount == 0) {
          orderBook.remove(bid.id)
          bids = bids.tail
        }

        if (ask.amount == 0) {
          orderBook.remove(ask.id)
          asks = asks.tail
        }
      }
    }
  }

  def start() {
    val listener = new ServerSocket(port)

    val serverActor = actor {
      loop {
        react {
          case (connectionId: Int, Login(username, password)) => handlersMap(connectionId) ! tryLogin(connectionId, username, password)
          case (connectionId: Int, Logout()) => logout(connectionId)
          case (connectionId: Int, command: ClientCommand) => handlersMap(connectionId) ! {
            if (!(usernamesMap contains connectionId)) LoginFailure() else {
              val username = usernamesMap(connectionId)
              val userData = users(username)
              command match {
                case Balance() => BalanceMessage(userData.balance)
                case Orders() => ClientOrdersList(orderBook.values.filter(o => o.username == username))
                case Buy(ticker, amount, price) =>
                  if (amount * price > userData.balance) OrderFailure() else {
                    orderBook += (lastOrderId -> new Order(lastOrderId, username, ticker, amount, price, BuyOrder))
                    lastOrderId += 1
                    OrderSuccess(lastOrderId - 1)
                  }
                case Sell(ticker, amount, price) =>
                  if (!(userData.assets contains ticker) || userData.assets(ticker) < amount) OrderFailure() else {
                    orderBook += (lastOrderId -> new Order(lastOrderId, username, ticker, amount, price, SellOrder))
                    lastOrderId += 1
                    OrderSuccess(lastOrderId - 1)
                  }
                case Cancel(orderId) => {
                  if (!(orderBook contains orderId)) CancelFailure() else {
                    val order = orderBook(orderId)
                    if (order.username != username) CancelFailure() else {
                      orderBook.remove(orderId)
                      CancelSuccess()
                    }
                  }
                }
              }
            }
          }
        }
      }
    }

    //Matches orders in the book every second
    //TODO: check for race conditions
    actor {
      while(true) {
        Thread.sleep(1000)
        matchOrders()
      }
    }

    //Listen for incoming connections and accept them with a separate client actor
    while(true) {
      val handler = new ClientHandler(listener.accept(), lastConnectionId, serverActor)
      handlersMap += (lastConnectionId -> handler)
      handler.start()

      lastConnectionId += 1
    }

    listener.close()
  }
}
