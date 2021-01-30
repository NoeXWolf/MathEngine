import org.scalatest.FunSuite
import fr.noexwolf.mathengine.vectors.Vector3n

class VectorTest extends FunSuite {

  test("Vector3n.equals") {
    val vector1 = Vector3n(1, 2, 3)
    val vector2 = Vector3n(1, 2, 3)
    assert(vector1 == vector2)
  }

  test("Vector3n.+") {
    val vector1 = Vector3n(1, 2, 3)
    val vector2 = Vector3n(4, 5, 6)
    val sum = vector1 + vector2
    assert(sum == Vector3n(5, 7, 9))
  }

  test("Vector3n.+=") {
    val vector1 = Vector3n(1, 2, 3)
    val vector2 = Vector3n(4, 5, 6)
    vector1 += vector2
    assert(vector1.x == 5 && vector1.y == 7 && vector1.z == 9)
  }

  test("Vector3n.-") {
    val vector1 = Vector3n(4, 2, 5)
    val vector2 = Vector3n(1, 6, 3)
    val diff = vector1 - vector2
    assert(diff == Vector3n(3, -4, 2))
  }

  test("Vector3n.-=") {
    val vector1 = Vector3n(4, 2, 5)
    val vector2 = Vector3n(1, 6, 3)
    vector1 -= vector2
    assert(vector1.x == 3 && vector1.y == -4 && vector1.z == 2)
  }

  test("Vector3n.*") {
    val vector = Vector3n(1, 2, 3)
    val scalar = 3
    val prod = vector * scalar
    assert(prod == Vector3n(3, 6, 9))
  }

  test("Vector3n.*=") {
    val vector = Vector3n(1, 2, 3)
    val scalar = 3
    vector *= scalar
    assert(vector == Vector3n(3, 6, 9))
  }

  test("Vector3n./") {
    val vector = Vector3n(6, 2, -4)
    val scalar = 3
    val quot = vector / scalar
    assert(quot == Vector3n(2, 2f/3, -4f/3))
  }

  test("Vector3n./=") {
    val vector = Vector3n(6, 2, -4)
    val scalar = 3
    vector /= scalar
    assert(vector == Vector3n(2, 2f/3, -4f/3))
  }

  test("Vector3n.dot") {
    val vector1 = Vector3n(1, 2, 3)
    val vector2 = Vector3n(4, 5, 6)
    val dotProd1 = vector1 dot vector2
    val dotProd2 = vector1 * vector2
    assert(dotProd1 == dotProd2 && dotProd1 == 32)
  }

  test("Vector3n.cross") {
    val vector1 = Vector3n(1, 2, 3)
    val vector2 = Vector3n(4, 5, 6)
    val crossProd1 = vector1 cross vector2
    val crossProd2 = vector1 % vector2
    assert(crossProd1 == crossProd2 && crossProd1 == Vector3n(-3, 6, -3))
  }

  test("Vector3n.%=") {
    val vector1 = Vector3n(1, 2, 3)
    val vector2 = Vector3n(4, 5, 6)
    vector1 %= vector2
    assert(vector1 == Vector3n(-3, 6, -3))
  }

  test("Vector3n.magnitude") {
    val vector = Vector3n(1, 2, 3)
    val magnitude = vector.magnitude()
    assert(magnitude == math.sqrt(14).asInstanceOf[Float])
  }

  test("Vector3n.normalize") {
    val vector = Vector3n(1, 2, 3)
    val mag = vector.magnitude()
    vector.normalize()
    assert(vector == Vector3n(1/mag, 2/mag, 3/mag))
  }

}
