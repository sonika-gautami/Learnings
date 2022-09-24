package test

object TrapppingRainWater extends App {

  println(trap(Array(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1)))

  def trap(height: Array[Int]): Int = {
    var left = 0
    var right = height.length - 1;
    var ans = 0
    var left_max = 0
    var right_max = 0

    /*
     For every left, if there is max value at right side
      - we can continue filling water at left for (maxLeft - currHeight)
     When the left side becomes higher than right-side,
      - we need to move right side as for right-side we can fill water
        as there is already a higher value at left with (maxRight - currHight)

     In either direction, we we hit the currHight > maxOfLeft/Right
     - we can't fill water there; so only max  we change no water-fill
     - we swap max as next to left/right can now store water upto this max height.
     */
    while (left < right) {
      println(s"$left - $right; $left_max - $right_max; $ans ; curr = ${height(left)} - ${height(right)}")
      if (height(left) < height(right)) {
        if (height(left) >= left_max)
          left_max = height(left)
        else ans += (left_max - height(left))
        left = left + 1;
      }
      else {
        if (height(right) >= right_max)
          right_max = height(right)
        else ans += (right_max - height(right));
        right = right - 1
      }
    }
    ans
  }
}
