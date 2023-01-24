import collection.mutable.Buffer
import Behaviour.*
import Utility.*

import scala.util.Random

// viewport width
val width = 200

// viewport height
val height = 150

val numberOfBoids = 15
// in radians
val FOV = math.Pi
// arbitrary range of view
val range = 10
val boids = Array.fill[Boid](numberOfBoids)()

// field of view
def inFOV (of:Boid, target:Boid) = of.heading.inArc(target.pos-of.pos,FOV)

def adjustToCenter (boid:Boid) = {

}


def nearestNeighbours (n: Int) = {

}

@main def main () = {

}