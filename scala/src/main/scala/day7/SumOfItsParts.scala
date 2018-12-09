package day7

import scala.io.Source

object SumOfItsParts {

  private val dependencyRegex = raw"Step ([A-Z]+) must be finished before step ([A-Z]+) can begin.".r

  def parseDependency(line: String): (Char, Char) = line match {
    case dependencyRegex(from, to) => (from(0), to(0))
  }

  case class Node(id: Char, destinations: List[Char] = List(), dependencies: Set[Char] = Set()) {
    def isDependenciesMet(visited: Set[Char]): Boolean = dependencies.subsetOf(visited)

    def withDestination(to: Char): Node = this.copy(destinations = (to :: destinations).sorted) // priority list?
    def withDependency(on: Char): Node = this.copy(dependencies = dependencies + on)
  }

  class Graph(val nodesByFrom: Map[Char, Node] = Map()) {
    def withEdge(from: Char, to: Char): Graph = {
      val updatedDestination = nodesByFrom + (from -> (nodesByFrom.get(from) match {
        case Some(node) => node.withDestination(to)
        case _ => Node(from, List(to))
      }))
      new Graph(updatedDestination)
    }

    def withDependencies: Graph = {
      val updated = nodesByFrom.values
        .flatMap(n => {
          n.destinations
            .map(n2 => nodesByFrom.get(n2) match {
              case Some(node) => node.withDependency(n.id)
              case None => Node(n2).withDependency(n.id)
            })
        })
        .groupBy(_.id)

      val updated2 = updated
        .mapValues(x =>
          x.head.copy(dependencies = x.flatMap(_.dependencies).toSet)
        ) ++ nodesByFrom.filter { case (id, node) => !updated.contains(id) }
      new Graph(updated2)
    }

  }

  def buildGraph(pairs: Seq[(Char, Char)]): Graph =
    pairs
      .foldLeft(new Graph()) {
        case (graph, (from, to)) => graph.withEdge(from, to)
      }
      .withDependencies

  def solution1(graph: Graph): String = {
    var visited: List[Char] = List()
    while (visited.size != graph.nodesByFrom.size) {
      val found = ('A' to 'Z')
        .filter(id => !visited.contains(id))
        .filter(id => graph.nodesByFrom.contains(id))
        .map(id => graph.nodesByFrom(id))
        .find(node => node.isDependenciesMet(visited.toSet))
      visited = visited ::: List(found.get.id)
    }
    visited.foldLeft("") {
      _ + _
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day7/input.txt").getLines
      .map(parseDependency)
      .toSeq
    val graph = buildGraph(lines)
    val nodesVisited = solution1(graph)
    println(nodesVisited)
  }

}
