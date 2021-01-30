package fr.noexwolf.mathengine.vectors

import scala.math._

case class Vector3n(var x: Float, var y: Float, var z: Float) {

  // ##### Operators ##### //

  // Add
  def +(vector: Vector3n): Vector3n = Vector3n(x + vector.x, y + vector.y, z + vector.z)

  def +=(vector: Vector3n): Unit = {
    x += vector.x
    y += vector.y
    z += vector.z
  }

  // Subtract
  def -(vector: Vector3n): Vector3n = Vector3n(x - vector.x, y - vector.y, z - vector.z)

  def -=(vector: Vector3n): Unit = {
    x -= vector.x
    y -= vector.y
    z -= vector.z
  }

  // Multiply
  def *(scalar: Float): Vector3n = Vector3n(x * scalar, y * scalar, z * scalar)

  def *=(scalar: Float): Unit = {
    x *= scalar
    y *= scalar
    z *= scalar
  }

  // Divide
  def /(scalar: Float): Vector3n = Vector3n(x / scalar, y / scalar, z / scalar)

  def /=(scalar: Float): Unit = {
    x /= scalar
    y /= scalar
    z /= scalar
  }

  // Dot Product
  def dot(vector: Vector3n): Float = x * vector.x + y * vector.y + z * vector.z

  def *(vector: Vector3n): Float = this dot vector

  // Cross Product
  def cross(vector: Vector3n): Vector3n = Vector3n(y * vector.z - z * vector.y, z * vector.x - x * vector.z, x * vector.y - y * vector.x)

  def %(vector: Vector3n): Vector3n = this cross vector

  def %=(vector: Vector3n): Unit = {
    val oldx = x
    val oldy = y
    val oldz = z
    x = oldy * vector.z - oldz * vector.y
    y = oldz * vector.x - oldx * vector.z
    z = oldx * vector.y - oldy * vector.x
  }

  // ##### Operators ##### //


  def magnitude(): Float = sqrt(pow(x, 2) + pow(y, 2) + pow(z, 2)).asInstanceOf[Float]

  def normalize(): Unit = {
    val magnitude = this.magnitude()
    x /= magnitude
    y /= magnitude
    z /= magnitude
  }

}
