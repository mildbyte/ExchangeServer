import scala.actors.Actor
import scala.collection.mutable.Map

object OrderType extends Enumeration {
  type OrderType = Value
  val BuyOrder, SellOrder = Value
}

//TODO: discrete tick size (problems with FP precicion otherwise)
import OrderType._
class Order(val id: Int, val username: String, val ticker: String, var amount: Int, val price: Double, val orderType: OrderType)

class OrderBook(users: Map[String, UserData], routerActor: Actor) {
  private val orderBook = Map[Int, Order]()     //order ID -> order data
  private val lastPrice = Map[String, Double]() //last executed price
  private var lastOrderId = 0                   //Each order has a unique ID

  def getLastPrice(ticker: String) = lastPrice.get(ticker)

  def tryCancel(orderId: Int, username: String) =
    if (!(orderBook contains orderId)) CancelFailure()
    else {
      val order = orderBook(orderId)
      if (order.username != username) CancelFailure()
      else {
        orderBook.remove(orderId)
        CancelSuccess()
      }
    }

  def tryBuy(amount: Int, price: Double, userData: UserData, username: String, ticker: String) =
    if (amount <= 0 || price <= 0 || amount * price > userData.balance) OrderFailure()
    else {
      orderBook += (lastOrderId -> new Order(lastOrderId, username, ticker, amount, price, BuyOrder))
      lastOrderId += 1
      OrderSuccess(lastOrderId - 1)
    }

  def trySell(userData: UserData, ticker: String, amount: Int, username: String, price: Double) =
    if (amount <= 0 || price <= 0 || !(userData.assets contains ticker) || userData.assets(ticker) < amount) OrderFailure()
    else {
      orderBook += (lastOrderId -> new Order(lastOrderId, username, ticker, amount, price, SellOrder))
      lastOrderId += 1
      OrderSuccess(lastOrderId - 1)
    }

  def getOrdersList(username: String) = orderBook.values.filter(o => o.username == username)

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
        routerActor ! (bid.username, Executed(bid.id, amount, price))
        routerActor ! (ask.username, Executed(ask.id, amount, price))

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

        //Update the last price at which the stock was traded
        lastPrice += (ticker -> price)
      }
    }
  }
}
