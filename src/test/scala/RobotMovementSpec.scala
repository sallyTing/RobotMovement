package robot

import org.scalatest.{BeforeAndAfter, FunSpec}

/**
  * Created by sally on 10/09/2017.
  */
class RobotMovementSpec extends FunSpec with BeforeAndAfter{

  val robotNotOnTable: Robot = Robot(-1, -1, "None")
  val robotOnTable: Robot = Robot(0,2,"WEST")

  describe("a robot which is not on table ") {
    it("will not be moved") {
      assert(RobotMovement.processMovement(robotNotOnTable, "MOVE") === robotNotOnTable)
    }

    it("will not turn left or right") {
      assert(RobotMovement.processMovement(robotNotOnTable, "LEFT") === robotNotOnTable)
    }

    it("will not be placed out of table") {
      assert(RobotMovement.processMovement(robotNotOnTable, "PLACE 0,5,NORTH") === robotNotOnTable)
    }

    describe("can be placed on the table") {
      it("will have the right position") {
        assert(RobotMovement.processMovement(robotNotOnTable, "PLACE 0,1,NORTH").x === 0)
      }

      it("will have the right direction") {
        assert(RobotMovement.processMovement(robotNotOnTable, "PLACE 0,1,NORTH").f === "NORTH")
      }
    }
  }

  describe("a robot on table") {
    it("can turn direction") {
      assert(RobotMovement.processMovement(robotOnTable,"LEFT").f === "SOUTH")
    }

    it("will not move out of table") {
      assert(RobotMovement.processMovement(robotOnTable,"MOVE") === robotOnTable)
    }
  }

}
