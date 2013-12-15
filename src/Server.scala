import java.net.ServerSocket
import scala.collection.mutable.Map
import scala.actors.Actor.{actor, loop, react}

class UserData(var password: String, var balance: Double, val assets: Map[String, Int])
//Assets: stock ticker -> amount held

class Server (port: Int) {
  val handlersMap = Map[Int, ClientHandler]() //connection ID -> handler thread
  val usernamesMap = Map[Int, String]()       //connection ID -> username
  val connectionsMap = Map[String, Int]()     //username -> connection ID
  val users = Map[String, UserData]()         //username -> password, balance, assets
  var lastConnectionId = 0                    //Each connection has a unique ID

  //Add some test users
  users += ("test" -> new UserData("test", 100.0, Map()))
  users += ("test2" -> new UserData("test", 0.0, Map("AAPL" -> 10)))

  private def tryLogin(connectionId: Int, username: String, password: String) =
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

  private def logout(connectionId: Int) {
    if (usernamesMap contains connectionId) {
      connectionsMap.remove(usernamesMap(connectionId))
      usernamesMap.remove(connectionId)
    }
  }

  def start() {
    val listener = new ServerSocket(port)

    //Relays messages directed to a username to the relevant handler thread, if it exists
    val clientRouter = actor {
      loop {
        react {
          case (username: String, message: ServerMessage) => if (connectionsMap contains username) {
            handlersMap(connectionsMap(username)) ! message
          }
        }
      }
    }

    val orderBook = new OrderBook(users, clientRouter)

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
                case Orders() => ClientOrdersList(orderBook.getOrdersList(username))
                case Buy(ticker, amount, price) => orderBook.tryBuy(amount, price, userData, username, ticker)
                case Sell(ticker, amount, price) => orderBook.trySell(userData, ticker, amount, username, price)
                case Cancel(orderId) => orderBook.tryCancel(orderId, username)
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
        orderBook.matchOrders()
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
