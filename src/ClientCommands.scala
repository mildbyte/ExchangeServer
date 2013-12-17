abstract class ClientCommand
sealed abstract class UserCommand extends ClientCommand
sealed abstract class AdminCommand extends ClientCommand

//TODO: fix weird naming caused by trying to avoid name conflicts between client commands and server responses
case class Buy(ticker: String, amount: Int, price: Double) extends UserCommand
case class Sell(ticker: String, amount: Int, price: Double) extends UserCommand
case class Cancel(orderId: Int) extends UserCommand
case class Login(username: String, password: String) extends UserCommand
case class Logout() extends UserCommand
case class Balance() extends UserCommand
case class Orders() extends UserCommand
case class LastPrice(ticker: String) extends UserCommand
case class HeldAmount(ticker: String) extends UserCommand

case class AddUser(username: String, password: String) extends AdminCommand
case class RemoveUser(username: String) extends AdminCommand
case class ChangeUserBalance(username: String, balance: Double) extends AdminCommand
case class SetUserAssets(username: String, ticker: String, amount: Int) extends AdminCommand

object ClientCommand {
  def parse(command: String) = {
    val tokens = command.trim.split(' ')
    if (tokens.length == 0) None else try tokens(0) match {
      case "BUY"        => Buy(tokens(1), tokens(2).toInt, tokens(3).toDouble)
      case "SELL"       => Sell(tokens(1), tokens(2).toInt, tokens(3).toDouble)
      case "CANCEL"     => Cancel(tokens(1).toInt)
      case "LOGIN"      => Login(tokens(1), tokens(2))
      case "LOGOUT"     => Logout()
      case "BALANCE"    => Balance()
      case "ORDERS"     => Orders()
      case "PRICE"      => LastPrice(tokens(1))
      case "ASSETS"     => HeldAmount(tokens(1))
      case "ADDUSER"    => AddUser(tokens(1), tokens(2))
      case "REMOVEUSER" => RemoveUser(tokens(1))
      case "SETBALANCE" => ChangeUserBalance(tokens(1), tokens(2).toDouble)
      case "SETASSETS"  => SetUserAssets(tokens(1), tokens(2), tokens(3).toInt)
      case _            => None
    } catch {
      case ex: NumberFormatException => None
      case ex: ArrayIndexOutOfBoundsException => None
    }
  }
}