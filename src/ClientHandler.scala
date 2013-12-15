import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.net.Socket
import scala.actors.Actor
import scala.actors.Actor.actor

class ClientHandler (socket: Socket, clientId: Int, serverActor: Actor) extends Actor {
  def act() {
    val in = new BufferedReader(new InputStreamReader(socket.getInputStream))
    val out = new PrintWriter(socket.getOutputStream, true)

    //Reads commands from the client, parses them and forwards to the server
    actor {
      var active = true

      while (active) {
        val command = in.readLine()
        if (command == null) {
          active = false
          serverActor ! (clientId, Logout())
        } else {
          val parsedCommand = ClientCommand.parse(command)

          //Quietly ignore invalid commands
          if (parsedCommand != None) {
            serverActor ! (clientId, parsedCommand)
            if (parsedCommand.isInstanceOf[Logout]) {
              active = false
              in.close()
              socket.close()
            }
          }
        }
      }
    }

    //Receives messages from the server and forwards them to the client
    loop {
      react {
        case message: ServerMessage => out.println(message.toString)
      }
    }

    socket.close()
  }
}
