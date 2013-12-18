import java.net.ServerSocket
import scala.collection.mutable.Map
import scala.actors.Actor.{actor, loop, react}

class UserLoginData(var isAdmin: Boolean, var password: String)
class UserAssetData(var balance: Double, val assets: Map[String, Int])
//Assets: stock ticker -> amount held

class Server (port: Int) {
  val handlersMap = Map[Int, ClientHandler]()   //connection ID -> handler thread
  val usernamesMap = Map[Int, String]()         //connection ID -> username
  val connectionsMap = Map[String, Int]()       //username -> connection ID
  val userAuth = Map[String, UserLoginData]()   //username -> password, permissions
  val userAssets = Map[String, UserAssetData]() //username -> balance, assets
  var lastConnectionId = 0                      //Each connection has a unique ID

  //Add some test users
  userAuth += ("test" -> new UserLoginData(false, "test"))
  userAuth += ("test2" -> new UserLoginData(false, "test"))
  userAuth += ("root" -> new UserLoginData(true, "root"))

  userAssets += ("test" -> new UserAssetData(100.0, Map()))
  userAssets += ("test2" -> new UserAssetData(0.0, Map("AAPL" -> 10)))
  userAssets += ("root" -> new UserAssetData(0.0, Map()))

  private def tryLogin(connectionId: Int, username: String, password: String) =
    if (
      (usernamesMap contains connectionId) ||
        (connectionsMap contains username) ||
        !(userAuth contains username) ||
        (userAuth(username).password != password)) LoginFailure()
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

    val orderBook = new OrderBook(userAssets, clientRouter)

    val serverActor = actor {
      loop {
        react {
          case (connectionId: Int, Login(username, password)) => handlersMap(connectionId) ! tryLogin(connectionId, username, password)
          case (connectionId: Int, Logout()) => logout(connectionId)
          case (connectionId: Int, command: ClientCommand) => handlersMap(connectionId) ! {
            if (!(usernamesMap contains connectionId)) LoginFailure() else {
              val username = usernamesMap(connectionId)
              val userData = userAssets(username)
              if (command.isInstanceOf[AdminCommand] && !userAuth(username).isAdmin) LoginFailure() else
              command match {
                case Balance() => BalanceMessage(userData.balance)
                case Orders() => ClientOrdersList(orderBook.getOrdersList(username))
                case Buy(ticker, amount, price) => orderBook.tryBuy(amount, price, userData, username, ticker)
                case Sell(ticker, amount, price) => orderBook.trySell(userData, ticker, amount, username, price)
                case Cancel(orderId) => orderBook.tryCancel(orderId, username)
                case LastPrice(ticker) => LastTradedPrice(orderBook.getLastPrice(ticker))
                case HeldAmount(ticker) =>
                  HeldAssetsMessage(if (userData.assets contains ticker) userData.assets(ticker) else 0)
                case AddUser(username, password) =>
                  if (userAuth contains username) AdminFailure() else {
                    userAuth += (username -> new UserLoginData(false, password))
                    userAssets += (username -> new UserAssetData(0.0, Map()))
                    AdminSuccess()
                  }
                case RemoveUser(username) =>
                  if (!(userAuth contains username) || (connectionsMap contains username)) AdminFailure() else {
                    //TODO: no way to kick a user out of the server
                    userAuth remove username
                    userAssets remove username
                    orderBook removeUser username
                    AdminSuccess()
                  }
                case ChangeUserBalance(username, balance) =>
                  if (!(userAuth contains username)) AdminFailure() else {
                    userAssets(username).balance = balance
                    AdminSuccess()
                  }
                case SetUserAssets(username, ticker, amount) =>
                  if (!(userAuth contains username)) AdminFailure() else {
                    userAssets(username).assets += (ticker -> amount)
                    if (amount == 0) userAssets(username).assets.remove(ticker)
                    AdminSuccess()
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
