sealed abstract class ServerMessage

case class OrderSuccess(orderId: Int) extends ServerMessage {
  override def toString = "OK " + orderId
}
case class OrderFailure() extends ServerMessage {override def toString = "FAIL"}
case class Executed(orderId: Int, amount: Int, price: Double) extends ServerMessage {
  override def toString = "DONE " + orderId + " " + amount + " " + price
}

case class CancelSuccess() extends ServerMessage {override def toString = "OK"}
case class CancelFailure() extends ServerMessage {override def toString = "FAIL"}

case class LoginSuccess() extends ServerMessage {override def toString = "OK"}
case class LoginFailure() extends ServerMessage {override def toString = "FAIL"}

case class BalanceMessage(balance: Double) extends ServerMessage {override def toString = balance toString}
case class ClientOrdersList(orders: Iterable[Order]) extends ServerMessage {
  override def toString = {
    (for (order <- orders) yield
      order.id + " " +
      (if (order.orderType == OrderType.BuyOrder) "BUY" else "SELL") + " " +
      order.amount + " " + order.ticker + " " + order.price).mkString("\n")
  }
}

case class LastTradedPrice(price: Option[Double]) extends ServerMessage {
  override def toString = price match {
    case Some(p) => p toString
    case None    => "NONE"
  }
}

case class HeldAssetsMessage(amount: Int) extends ServerMessage {override def toString = amount toString}