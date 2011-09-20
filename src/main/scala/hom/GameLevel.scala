package hom

import java.io.{File,FileWriter}
import scala.xml.{Elem,Node,Unparsed,XML}

import Directions._
import LineColors._

import XMLable._

object GameLevel {
  def apply(resource: Resource, level: Int): GameLevel = {
    require(resource != null)
    fromXML(resource.loadXML(), level)
  }

  def fromXML(topElem: Elem, level: Int): GameLevel = {

    def parseGates(node: Node, typeName: String) = (node \\ typeName)(0).descendant.map(_ match {
      case s @ <Source/> => { val u: UnXMLable[FixedSource] = s; u.fromXML }
      case s @ <Conduit/> => { val u: UnXMLable[FixedConduit] = s; u.fromXML }
      case s @ <Mirror/> => { val u: UnXMLable[FixedMirror] = s; u.fromXML }
      case s @ <Prism/> => { val u: UnXMLable[FixedPrism] = s; u.fromXML }
      case s @ <PartialMirror/> => { val u: UnXMLable[FixedPartialMirror] = s; u.fromXML }
      case s @ <CrossMirror/> => { val u: UnXMLable[FixedCrossMirror] = s; u.fromXML }
      case s @ <Detector/> => { val u: UnXMLable[FixedDetector] = s; u.fromXML }
      case s @ <Blocker/> => { val u: UnXMLable[FixedBlocker] = s; u.fromXML }
      case s @ <WormHole/> => { val u: UnXMLable[FixedWormHole] = s; u.fromXML }
    })

    def parseMoveableGates(node: Node, typeName: String) = (node \\ typeName)(0).descendant.map(_ match {
      case s @ <Source/> => { val u: UnXMLable[MoveableSource] = s; u.fromXML }
      case s @ <Conduit/> => { val u: UnXMLable[MoveableConduit] = s; u.fromXML }
      case s @ <Mirror/> => { val u: UnXMLable[MoveableMirror] = s; u.fromXML }
      case s @ <Prism/> => { val u: UnXMLable[MoveablePrism] = s; u.fromXML }
      case s @ <PartialMirror/> => { val u: UnXMLable[MoveablePartialMirror] = s; u.fromXML }
      case s @ <CrossMirror/> => { val u: UnXMLable[MoveableCrossMirror] = s; u.fromXML }
      case s @ <Detector/> => { val u: UnXMLable[MoveableDetector] = s; u.fromXML }
      case s @ <Blocker/> => { val u: UnXMLable[MoveableBlocker] = s; u.fromXML }
      case s @ <WormHole/> => { val u: UnXMLable[MoveableWormHole] = s; u.fromXML }
    })

    def parseBound(node: Node): Bound = {
      val myBoundsNode = (node \\ "bounds")(0)
      val u: UnXMLable[Bound] = myBoundsNode
      u.fromXML
    }

    val trimTop = scala.xml.Utility.trim(topElem)
    val bounds = parseBound((trimTop \\ "bounds")(0))

    val fixedGates = parseGates(trimTop, "fixedGates")
    val moveableGates = parseMoveableGates(trimTop, "moveableGates")
    println("fixed " + fixedGates.length)
    println("moveable " + moveableGates.length)

    def repairHoles(fixedGates: List[Gate], moveableGates: List[Gate]): Tuple2[List[Gate], List[Gate]] = {
      import collection.mutable.{HashSet, ListBuffer}
      val allHoles = new HashSet[WormHole]
      allHoles ++= (fixedGates ::: moveableGates) filter (_.isInstanceOf[WormHole]) map (_.asInstanceOf[WormHole])
      if (!allHoles.isEmpty) {
        println(allHoles.size +" holes")
        val allPairs = ListBuffer[Tuple2[WormHole, WormHole]]()
        val losers = new HashSet[WormHole]
        // find pairs
        while (!allHoles.isEmpty) {
          val one = allHoles.iterator.next()
          val two = allHoles.find(_.position == one.twin.position).getOrElse(throw new IllegalStateException("Missing wormhole terminating at "+ one.position))
          allPairs += Tuple2(one, two)
          allHoles -= one
          allHoles -= two
          losers += one
          losers += two
        }
        println(allPairs.size +" pairs")
        // recreate paired wormholes
        val newHoles = allPairs flatMap { t => makeWormHoles(t._1, t._2) }
        println(newHoles.size +" new holes")
        val (newMoveable, newFixed) = newHoles partition (_.isInstanceOf[Moveable])
        println(newMoveable.size +" are moveable, fixed are "+ newFixed.size)
        // all holes are replaced, so the filter removes all holes
        val fixedUpdated = (fixedGates filter (g => !g.isInstanceOf[WormHole] || !losers.contains(g.asInstanceOf[WormHole]))) ++ newFixed
        val moveableUpdated = (moveableGates filter (g => !g.isInstanceOf[WormHole] || !losers.contains(g.asInstanceOf[WormHole]))) ++ newMoveable
        (fixedUpdated, moveableUpdated)
        //(fixedGates filter (losers.contains(_)) ++ newFixed, moveableGates filter (losers.contains(_)) ++ newMoveable)
      } else {
        println("No holes to repair")
        (fixedGates, moveableGates)
      }
    }
    def makeWormHoles(a: WormHole, b: WormHole): Seq[WormHole] = {
      require(a.position == b.twin.position && a.twin.position == b.position)
      val anchor = if (a.isInstanceOf[FixedWormHole]) {
        new FixedWormHole(a.position, b.position, b.isInstanceOf[FixedWormHole])
      } else {
        new MoveableWormHole(a.position, b.position, b.isInstanceOf[FixedWormHole])
      }
      List(anchor, anchor.twin)
    }
    val (fixedGatesRepaired, moveableGatesRepaired) = repairHoles(fixedGates, moveableGates)

    /*
    // wormholes don't know their twins yet; for fun, handle the case of one is moveable but the other is fixed
    for (one <- fixedGates if (one.isInstanceOf[WormHole] && one.asInstanceOf[WormHole].twin == null)) {
      val cur = one.asInstanceOf[WormHole]
      // look among other fixed gates (self-test should fail)
      for (two <- fixedGates if (two.isInstanceOf[WormHole] && two.asInstanceOf[WormHole].position == cur.other)) {
        val other = two.asInstanceOf[WormHole]
        cur.twin = other
        other.twin = cur
      }
      // look among moveable gates
      if (cur.twin == null) {
        for (two <- moveableGates if (two.isInstanceOf[WormHole] && two.asInstanceOf[WormHole].position == cur.other)) {
          val other = two.asInstanceOf[WormHole]
          cur.twin = other
          other.twin = cur
        }
      }
    }
    // any remaining are pairs which are both moveable
    for (one <- moveableGates if (one.isInstanceOf[WormHole] && one.asInstanceOf[WormHole].twin == null)) {
      val cur = one.asInstanceOf[WormHole]
      // look among other moveable gates (self-test should fail)
      for (two <- moveableGates if (two.isInstanceOf[WormHole] && two.asInstanceOf[WormHole].position == cur.other)) {
        val other = two.asInstanceOf[WormHole]
        cur.twin = other
        other.twin = cur
      }
    }
    */

    println("final fixed " + fixedGatesRepaired.length)
    println("final moveable " + moveableGatesRepaired.length)
    val description = (trimTop \\ "description")(0).child.foldLeft("")(_ + _.toString)
    return new GameLevel(level, description, bounds, fixedGatesRepaired, moveableGatesRepaired)
  }

  def toXML(g: GameLevel): Elem = {
    <houseofmirrors >
      <description>{Unparsed(g.description)}</description>
      <bounds width={g.bounds.width.toString} height={g.bounds.height.toString} />
      <fixedGates>{g.fixedGates.map(_.toXML)}</fixedGates>
      <moveableGates>{g.moveableGates.map(_.toXML)}</moveableGates>
    </houseofmirrors>
  }
}

/**
 * Models a game level as a grid with gate objects.
 * Moveable gates can be moved and rotated.
 * Current board state is generated by trace, which calculates light paths as line segments and current gate states.
 */
case class GameLevel(level: Int, description: String, bounds: Bound, fixedGates: List[Gate], moveableGates: List[Gate]) {

  val gates = fixedGates ::: moveableGates

  /**
   * A chance to free resources on shutdown.
   */
  def dispose() {
    //empty
  }

  def occupied(p: Point) = {
    this.gates.find(g => g.position == p).isDefined
  }

  private def operateAt(p: Gate => Boolean)(f: Gate => Gate): Option[GameLevel] = {
    var found: Boolean = false
    val result = moveableGates map(g => if (p(g)) { found = true; println("Op on "+ g); f(g)} else g)
    if (found) {
      // validation allows the other end of a worm hole to drop out in favor of replacement twin
      val validated = result map (_.validate(fixedGates ::: result))
      Some(copy(moveableGates = validated))
    } else {
      None
    }
  }

  def turnCCW(p: Point): Option[GameLevel] = operateAt(g => g.position == p && g.isInstanceOf[Turnable]) { _.asInstanceOf[Turnable].turnCCW }

  def turnCW(p: Point): Option[GameLevel] = operateAt(g => g.position == p && g.isInstanceOf[Turnable]) { _.asInstanceOf[Turnable].turnCW }

  def moveUp(p: Point) = moveTo(p, p.moveUp)

  def moveDown(p: Point) = moveTo(p, p.moveDown)

  def moveLeft(p: Point) = moveTo(p, p.moveLeft)

  def moveRight(p: Point) = moveTo(p, p.moveRight)

  def moveTo(from: Point, target: Point): Option[GameLevel] = {
    if (this.bounds.contains(target) && !occupied(target)) {
      operateAt(g => g.contains(from) && g.isInstanceOf[Moveable]) { _.asInstanceOf[Moveable].moveTo(target) }
    } else {
      None
    }
  }

  private def sourceRays: List[Ray] = gates filter (_.isInstanceOf[Source]) flatMap (_.asInstanceOf[Source].emit)

  /** Derive the light rays and gate states (i.e., whether Detectors are detecting) from the current board model. */
  def trace: GameState = {
    val (segments, revisedGates) = shootRayList(sourceRays, this.gates, Nil)
    //println(segments.length + " Trace segments " + segments)
    val status = this.status(revisedGates)
    return new GameState(segments, revisedGates, status)
  }

  private def status(g: List[Gate]): StatusUpdate = {
    val detectors = g.filter (_.isInstanceOf[Detector]).map(_.asInstanceOf[Detector])
    val numOn = detectors.foldLeft(0)((sum, d) => if (d.isOn) sum + 1 else sum)
    val totalDetectors = detectors.length
    return new StatusUpdate(this.description, numOn, totalDetectors)
  }

  /** Shoots a ray, and gives a list of resulting line-segments */
  private def shootRay(r: Ray, gateList: List[Gate], visitedGates: List[(Gate, Direction, LineColor)]): (List[Segment], List[Gate]) = {
    var traceSegment : List[Segment] = Nil
    var traceGate : List[Gate] = gateList

    var prevPoint = r.start
    var point = r.nextPoint(prevPoint)
    var done = false
    while (!done && this.bounds.containsOneOff(point)) {
      gateList.find(g => g.position == point) match {
        case Some(gate) => {
          def addUniqueGate(gate: Gate, gates: List[Gate]) = gate :: gates.filterNot(g => g.position == gate.position)

          if (!visitedGates.contains((gate, r.direction, r.color))) {
            val (rays, newgate) = gate.act(r)

            //println(gate + " shoots " + rays + " and becomes " + newgate)
            traceGate = addUniqueGate(newgate, traceGate)

            val returnList = shootRayList(rays, traceGate, (gate, r.direction, r.color) :: visitedGates)
            traceSegment = returnList._1
            traceGate = returnList._2
          }
          prevPoint = point
          done = true
        }
        case None => {
          prevPoint = point
          point = r.nextPoint(point)
        }
      }
    }
    val returnGates = gateList.filterNot(x => traceGate.find(y => y.position == x.position).isDefined)
    (new Segment(r.start, prevPoint, r.color) :: traceSegment , traceGate ::: returnGates)
  }

  private def shootRayList(rays: List[Ray], gateList: List[Gate], visitations: List[(Gate, Direction, LineColor)]) = {
    var segments: List[Segment] = Nil
    var revisedGates: List[Gate] = gateList

    rays.foreach(ray => {
      val (resultSegments, resultGates) = shootRay(ray, revisedGates, visitations)
      segments = resultSegments ::: segments
      revisedGates = resultGates
    })
    (segments, revisedGates)
  }

  def toXML(): Elem = GameLevel.toXML(this)
}

case class GameState(segments: List[Segment], gates: List[Gate], status: StatusUpdate)

case class StatusUpdate(description: String, score: Int, total: Int) {
  def isComplete: Boolean = score == total
}

