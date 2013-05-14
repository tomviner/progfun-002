package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level0 extends SolutionChecker {
      /* terrain for level 0*/

    val level =
    """------
      |--ST--
      |--oo--
      |--oo--
      |------""".stripMargin

    val optsolution = List(Down, Right, Up)
  }
  test("optimal solution for level 0") {
    new Level0 {
      assert(solve(solution) === Block(goal, goal))
    }
  }
    test("solution for level 0") {
    new Level0 {
      assert(solution === optsolution)
    }
  }
  test("optimal solution length for level 0") {
    new Level0 {
      assert(solution.length === optsolution.length)
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


  trait Level6 extends SolutionChecker {
      /*terrain for level 6*/

    val level =
    """-----oooooo
      |-----o--ooo
      |-----o--ooooo
      |Sooooo-----oooo
      |----ooo----ooTo
      |----ooo-----ooo
      |------o--oo
      |------ooooo
      |------ooooo
      |-------ooo""".stripMargin

    val optsolution = List(Right, Right, Right, Down, Right, Down, Down, Right, Down, Down, Right, Up, Left, Left, Left, Up, Up, Left, Up, Up, Up, Right, Right, Right, Down, Down, Left, Up, Right, Right, Down, Right, Down, Down, Right)
  }
  test("optimal solution for level 6") {
    new Level6 {
      assert(solve(solution) === Block(goal, goal))
    }
  }
  test("solution for level 6") {
    new Level6 {
      assert(solution === optsolution)
    }
  }
  test("optimal solution length for level 6") {
    new Level6 {
      assert(solution.length === optsolution.length)
    }
  }


  trait Level11 extends SolutionChecker {
      /*terrain for level 11*/

    val level =
    """-oooo
      |-oToo
      |-ooo
      |-o---oooooo
      |-o---oo--oo
      |Soooooo--ooo
      |-----o-----o
      |-----oooo--o
      |-----ooooooo
      |--------ooo
    """.stripMargin

    val optsolution = List(Right, Right, Right, Right, Up, Left, Down, Down, Down, Right, Right, Right, Down, Left, Up, Left, Left, Left, Up, Up, Right, Up, Right, Right, Down, Right, Up, Left, Left, Left, Down, Down, Left, Left, Left, Up, Up, Right, Up, Up, Left, Down, Right, Up, Right, Down, Left)
  }
  test("optimal solution for level 11") {
    new Level11 {
      assert(solve(solution) === Block(goal, goal))
    }
  }
  test("solution for level 11") {
    new Level11 {
      assert(solution === optsolution)
    }
  }
  test("optimal solution length for level 11") {
    new Level11 {
      assert(solution.length === optsolution.length)
    }
  }


  // trait LevelX extends SolutionChecker {
  //      terrain for level X

  //   val level =
  //   "".stripMargin

  //   val optsolution = List()
  // }
  // test("optimal solution for level X") {
  //   new LevelX {
  //     assert(solve(solution) === Block(goal, goal))
  //   }
  // }
  // test("optimal solution length for level X") {
  //   new LevelX {
  //     assert(solution.length === optsolution.length)
  //   }
  // }



  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("isStanding function level 1") {
    new Level1 {
      assert(Block(Pos(1, 2), Pos(1, 2)).isStanding)
      assert(!Block(Pos(1, 2), Pos(1, 3)).isStanding)
      assert(startBlock.down.isStanding === false)
      assert(startBlock.down.down.isStanding === true)
    }
  }

  test("startPos level 1") {
    new Level1 {
      assert(startPos === Pos(1,1))
    }
  }

  test("startBlock level 1") {
    new Level1 {
      assert(startBlock.b1 === Pos(1,1))
      assert(startBlock.b2 === Pos(1,1))
    }
  }

  test("isLegal level 1") {
    new Level1 {
      assert(startBlock.down.isLegal === true)
      assert(startBlock.down.down.isLegal === false)
    }
  }

  test("neighbors level 1") {
    new Level1 {
      assert(startBlock.neighbors.toSet ===
        Set(
            (Block(Pos(1, -1), Pos(1, 0)), Left),
            (Block(Pos(1, 2), Pos(1, 3)), Right),
            (Block(Pos(2, 1), Pos(3, 1)), Down),
            (Block(Pos(-1, 1), Pos(0, 1)), Up)
        )
      )
    }
  }

  test("legalNeighbors level 1") {
    new Level1 {
      assert(startBlock.legalNeighbors.toSet ===
        Set(
            (Block(Pos(1, 2), Pos(1, 3)), Right),
            (Block(Pos(2, 1), Pos(3, 1)), Down)
        )
      )
    }
  }

  // solver tests

  test("done level 1") {
    new Level1 {
      val wrong_solution = List(Right, Right)
      assert(done(solve(optsolution)) === true)
      assert(done(solve(optsolution).up) === false)
      assert(done(solve(optsolution).up.down) === true)
      assert(done(solve(wrong_solution)) === false)
    }
  }

  test("neighborsWithHistory level 1") {
    new Level1 {
      assert(neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up)).toSet ===
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        )
      )
    }
  }

  test("pathsFromStart level 0") {
    new Level0 {
      assert(
        pathsFromStart.map(_._2).toList
        ===
        List(
          List(),
          List(Down),
          List(Right, Down),
          List(Up, Right, Down)
        )
      )
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) === Block(goal, goal))
    }
  }
  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length === optsolution.length)
    }
  }
}
