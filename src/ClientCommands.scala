sealed abstract class ClientCommand

case class Buy(ticker: String, amount: Int, price: Double) extends ClientCommand
case class Sell(ticker: String, amount: Int, price: Double) extends ClientCommand
case class Cancel(orderId: Int) extends ClientCommand
case class Login(username: String, password: String) extends ClientCommand
case class Logout() extends ClientCommand
case class Balance() extends ClientCommand
case class Orders() extends ClientCommand

object ClientCommand {
  def parse(command: String) = {
    val tokens = command.trim.split(' ')
    if (tokens.length == 0) None else try tokens(0) match {
      case "BUY"     => Buy(tokens(1), tokens(2).toInt, tokens(3).toDouble)
      case "SELL"    => Sell(tokens(1), tokens(2).toInt, tokens(3).toDouble)
      case "CANCEL"  => Cancel(tokens(1).toInt)
      case "LOGIN"   => Login(tokens(1), tokens(2))
      case "LOGOUT"  => Logout()
      case "BALANCE" => Balance()
      case "ORDERS"  => Orders()
      case _         => None
    } catch {
      case ex: NumberFormatException => None
      case ex: ArrayIndexOutOfBoundsException => None
    }
  }
}