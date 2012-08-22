package de.jens.actors
import akka.actor.Actor
import de.jens.Binding
import akka.actor.ActorRef

case class Message(bindings: Iterator[Binding])

class ProjectionActor(parent: ActorRef) extends Actor {
  
  def receive = {
    case Message(bindings) => parent ! Message(bindings)
  }

}