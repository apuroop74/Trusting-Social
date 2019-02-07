package spark

import scala.io.StdIn._
import scala.util.Random._

object query1 {

  /////////////////////A Method to create a matrix with random integer values as the elements//////////////////////////////////
  /*Assumption taken that each element of the matrix lies between 0 and 100 (0 inclusive and 100 exclusive)
       * If no limit to be considered then "nextInt(10)" in the below definition could be replaced by "nextInt()"
       */
  def createMatrix(rows: Int, columns: Int): Array[Array[Int]] = {

    var matrix = Array.ofDim[Int](rows, columns)

    //By default an array of dimensions rowsxcolumns is created with each element as 0

    var filler = scala.util.Random

    for {
      p <- 0 until rows
      q <- 0 until columns
    } matrix(p)(q) = filler.nextInt(10)

    matrix
  }

  // A method to print all the elements in an array
  // Inputs : Matrix, No.of rows, No. of columns
  // Output : matrix is printed. No return value
  def printMatrix(matrix: Array[Array[Int]], rows: Int, columns: Int): Unit = {

    val upper_limit = columns - 1

    for {
      p <- 0 until rows
      q <- 0 until columns
    } {
      q match {
        case `upper_limit` => println(matrix(p)(q))
        case _             => print(matrix(p)(q) + " ")
      }
    }

  }

  def sumMatrix(initial_matrix: Array[Array[Int]], rows: Int, columns: Int): Array[Array[Int]] = {

    var matrix = initial_matrix

    for (column <- 1 until columns)
      matrix(0)(column) += matrix(0)(column - 1)

    for (row <- 1 until rows)
      matrix(row)(0) += matrix(row - 1)(0)

    for {
      row <- 1 until rows
      column <- 1 until columns
    } {
      matrix(row)(column) += matrix(row)(column - 1) + matrix(row - 1)(column) - matrix(row - 1)(column - 1)
    }

    matrix
  }

  def main(args: Array[String]) {

    //    First Line User Input -> Dimensions of Matrix
    val inputDimensions = readLine()
    val matrix_dimensions = inputDimensions.split(" ").map(_.toInt)

    if (matrix_dimensions.size != 2) {
      println("We allow only 2D Matrix. Please try again")
    }

    val rows = matrix_dimensions(0)
    val columns = matrix_dimensions(1)

    val matrix = createMatrix(rows, columns)
    var init = matrix.map(_.clone())

    println("matrix created")

    val sum_matrix = sumMatrix(init, rows, columns)


    //A closure to create the sub-matrix using the given co-ordinates
    // Closure is considered because it was mentioned that only one matrix is available in the website. Hence, a matrix input is not required to create the function.
    //    def getSumInSubMatrix = (x1: Int, y1: Int, x2: Int, y2: Int) => {
    //
    ////      var sub_matrix = Array.ofDim[Int](x2 - x1 + 1, y2 - y1 + 1)
    //      var sum = 0
    //      for {
    //        p <- x1 until x2 + 1
    //        q <- y1 until y2 + 1
    //      } {
    ////        sub_matrix(p - x1)(q - y1) = matrix(p)(q)
    //        sum+=matrix(p)(q)
    //      }
    //
    //      sum
    //    }

    //A closure to check if the given co-ordinates are valid for the matrix present in the website

    def checkCoords = (x1: Int, y1: Int, x2: Int, y2: Int) => {

      if ((x1 <= x2) && (y1 <= y2) && (x1 >= 0) && (x1 < rows) && (x2 >= 0) && (x2 < rows) && (y1 >= 0) && (y1 < columns) && (y2 >= 0) && (y2 < columns))
        true
      else
        false

    }

    val noOfRequests = readInt()
    var i = 0
    var responseArray = Array.ofDim[Int](noOfRequests)

    while (i < noOfRequests) {
      val co_ordinates_input = readLine().split(" ").map(_.toInt)
      if (co_ordinates_input.size != 4) {
        println("invalid input")
        System.exit(1)
      }

      if (checkCoords(co_ordinates_input(0), co_ordinates_input(1), co_ordinates_input(2), co_ordinates_input(3))) {
        if (co_ordinates_input(0) == 0 && co_ordinates_input(1) == 0)
          responseArray(i) = sum_matrix(co_ordinates_input(2))(co_ordinates_input(3))
        else if ((co_ordinates_input(1) - 1) < 0)
          responseArray(i) = sum_matrix(co_ordinates_input(2))(co_ordinates_input(3)) - sum_matrix(co_ordinates_input(0) - 1)(co_ordinates_input(3))
        else if ((co_ordinates_input(0) - 1) < 0)
          responseArray(i) = sum_matrix(co_ordinates_input(2))(co_ordinates_input(3)) - sum_matrix(co_ordinates_input(2))(co_ordinates_input(1) - 1)
        else
          responseArray(i) = sum_matrix(co_ordinates_input(2))(co_ordinates_input(3)) - sum_matrix(co_ordinates_input(2))(co_ordinates_input(1) - 1) - sum_matrix(co_ordinates_input(0) - 1)(co_ordinates_input(3)) + sum_matrix(co_ordinates_input(0) - 1)(co_ordinates_input(1) - 1)
      } else {
        print("A sub-matrix is not possible with the given co-ordinates. ")
        if (i == noOfRequests - 1) {
          print("Exiting the application")
          System.exit(1)
        } else {
          println("please try again")
          i = i - 1
        }
      }

      i += 1
    }

    println("Matrix considered is :")
    printMatrix(matrix, rows, columns)

    //    println("")

    for (r <- 0 until noOfRequests)
      println(responseArray(r))

  }

}