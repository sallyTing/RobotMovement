package robot

import scala.io.StdIn

/**
  * Created by sally on 09/09/2017.
  */
object RobotMovement {
  val directions = List("SOUTH", "EAST", "NORTH", "WEST")

  def main(args: Array[String]): Unit = {

    println("Please input movement")
    startRound(Robot(-1, -1, "None"))
    def startRound(robot: Robot): Unit = {
      val action = StdIn.readLine("your input : ")
      action match {
        case "REPORT" if onTable(robot) => {
          println("Output: "+robot.x+","+robot.y+","+robot.f)
        }
        case _ => startRound(processMovement(robot, action.trim))
      }
    }

  }

  def processMovement(robot: Robot, action: String): Robot = {
    action match {
      case actionString if actionString.takeWhile(c => c != ' ') == "PLACE" =>
        val locationList = action.dropWhile(c => c != ' ').trim.split(",").map(_.trim).toList
        locationList match {
          case (xString :: yString :: f :: Nil) =>
            val x = try {
              xString.toInt
            } catch {
              case e: Exception => -1
            }
            val y = try {
              yString.toInt
            } catch {
              case e: Exception => -1
            }
            val newRobot = Robot(x, y, f)
            if (onTable(newRobot)) newRobot
            else robot
          case _ => robot
        }
      case actionString if !onTable(robot) => robot
      case "LEFT" => Robot(robot.x, robot.y, changeDirection(robot.f, true))
      case "RIGHT" => Robot(robot.x, robot.y, changeDirection(robot.f, false))
      case "MOVE" => moveForward(robot)
      case _ => robot
    }
  }

  def changeDirection(dire: String, left: Boolean): String = {
    val change = if (left) 1 else -1
    val newIndex = (directions.indexOf(dire) + change) match {
      case -1 => directions.length -1
      case 4 => 0
      case _ => directions.indexOf(dire) + change
    }
    directions(newIndex)
  }

  def moveForward(robot: Robot): Robot = {
    val newRobot = robot.f match {
      case "SOUTH" => Robot(robot.x, robot.y-1, robot.f)
      case "EAST" => Robot(robot.x+1, robot.y, robot.f)
      case "NORTH" => Robot(robot.x, robot.y+1, robot.f)
      case "WEST" => Robot(robot.x-1, robot.y, robot.f)
    }
    if (onTable(newRobot)) newRobot
    else robot
  }

  def onTable(robot: Robot): Boolean = {
    if (robot.x < 0 || robot.x > 4 || robot.y < 0 || robot.y > 4 || directions.indexOf(robot.f) == -1) false
    else true
  }
}
  case class Robot (
                   x: Int,
                   y: Int,
                   f: String
                   )
