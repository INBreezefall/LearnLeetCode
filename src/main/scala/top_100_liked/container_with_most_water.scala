package com.inbreeze
package top_100_liked

/**
 * {{{
 * Link: https://leetcode.cn/problems/container-with-most-water/?envType=study-plan-v2&envId=top-100-liked
 *
 * 题目原文:
 * 11. 盛最多水的容器
 *     给定一个长度为 n 的整数数组 height 。有 n 条垂线，第 i 条线的两个端点是 (i, 0) 和 (i, height[i]) 
 *     找出其中的两条线，使得它们与 x 轴共同构成的容器可以容纳最多的水。
 *     返回容器可以储存的最大水量
 *
 * 题目理解:
 *     [个解]1.构建 二元组 (value,index) 按 value 排序, 暴力双 For 循环
 *     [个解]2.双指针遍历 小的那侧向内移动指针, 终止条件 i >= j
 *
 * 关键字抽取:
 *     两条竖线 + X 轴构成的容器 => 面积公式 = min(h[i], h[j]) * (j - i)
 *        => 宽度 j - i 是单调递减, 单调递减 就意味着 使用 双指针(头尾指针) 向内移动
 *        => 由 期望公式(面积公式) 得 唯一有希望令公式结果增大的变量在于 min(h[i], h[j])
 *           即 变化 指向的较小数字的指针
 * }}}
 */
object container_with_most_water {
  private val example: Array[Int] = Array(1,8,6,2,5,4,8,3,7)
  private val example_output: Int = 49
  
  private def maxArea(height: Array[Int]): Int = {
    var left = 0
    var right: Int = height.length - 1
    var currentProductValue = 0
    var maxProductValue = 0

    // 0.终止条件 left >= right 左右指针相交
    while(left < right) {
      // 1.计算乘积值
      currentProductValue = if (height(left) > height(right)) {
        height(right) * (right - left)
      } else {
        height(left) * (right - left)
      }
      // 2.更新最大乘积值
      if (currentProductValue > maxProductValue)
        maxProductValue = currentProductValue
      // 3.指针移动
      if (height(left) >= height(right)) {
        right = right - 1
      } else {
        left = left + 1
      }
    }

    maxProductValue
  }

  def main(args: Array[String]): Unit = {
    println(maxArea(example))
  }
}
