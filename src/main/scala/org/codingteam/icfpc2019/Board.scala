package main.scala.org.codingteam.icfpc2019

import org.codingteam.icfpc2019.{Pos, Task}

case class Board(task : Task, bot : Bot, wrappedCells : Set[Pos])
