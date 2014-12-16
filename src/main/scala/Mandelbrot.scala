/**
 * Created by Zach Price on 12/16/14.
 */
package com.zach

import scala.math.pow
import scala.math.sqrt
import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter

object Mandelbrot {

  val INFINITY_THRESHOLD = 1e+100
  val ITERATIONS = 23
  val X_SIZE = 2
  val Y_SIZE = 1
  val SCALE = 400.0
  // If you increase MANDELBROT_EXPONENT above 2, you will get multibrot sets
  val MANDELBROT_EXPONENT = 2

  def generate: String = {
    val grid = buildGrid(X_SIZE, Y_SIZE, SCALE)
    val gridTF: Array[Array[Boolean]] = grid.map(row => row.map(iterate(_) < INFINITY_THRESHOLD))
    draw(gridTF)
  }

  // Calculates the value of the Mandelbrot iterated polynomial after ITERATIONS steps
  def iterate(c: Complex, z: Complex = new Complex(0,0), iteration: Int = 1): Double = {
    if(iteration > ITERATIONS) {
      return z.abs
    }
    iterate(c, (z^MANDELBROT_EXPONENT) + c, iteration + 1)
  }

  // Builds a grid of size @x * 2 * @scale x @y * 2 * @scale
  // Fills the grid with points in the complex plane where the bounds are (-@x,@x) (-@y, @y)
  def buildGrid(x: Int, y: Int, scale: Double): Array[Array[Complex]] = {
    Array.tabulate(y * 2 * scale.toInt, x * 2 * scale.toInt)((row, col) => new Complex(col/scale - x, row/scale - y))
  }

  // Takes a @gridTF of Booleans and returns a String of *'s corresponding to True positions
  def draw(gridTF: Array[Array[Boolean]]): String = {
    val gridChar = gridTF.map(row => row.map(if(_) "*" else " "))
    gridChar.foldLeft("") { (str, row) => str + row.foldLeft("") { (str, char) => str + char } + "\n"}
  }

  // Writes string to a file
  def write(filename: String): Unit = {
    val output = new File(filename)
    val writer = new BufferedWriter(new FileWriter(output))
    writer.write(generate)
    writer.flush()
    writer.close()
  }
}

// Define Complex class for complex numbers
// Has +, *, and ^ operators for addition, multiplication, and integer powers
// abs for calculating length/absolute value of a complex number
class Complex(real: Double, img: Double) {

  val a: Double = real
  val b: Double = img

  def +(num: Complex): Complex = {
    new Complex(this.a + num.a, this.b + num.b)
  }

  def *(num: Complex): Complex = {
    new Complex( (this.a * num.a) - (this.b * num.b), (this.a * num.b) + (this.b * num.a))
  }

  def ^(exp: Int): Complex = {
    if(exp <= 0)
      throw new Exception("Non-positive exponent")
    Iterator.fill(exp)(this).fold(new Complex(1,0)) { (p: Complex, v: Complex) => p*v }
  }

  def abs: Double = {
    sqrt(pow(this.a, 2) + pow(this.b, 2))
  }
}

