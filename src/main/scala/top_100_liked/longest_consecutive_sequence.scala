package com.inbreeze
package top_100_liked

import scala.collection.mutable

/**
 * {{{
 * Link: https://leetcode.cn/problems/longest-consecutive-sequence/?envType=study-plan-v2&envId=top-100-liked
 * 
 * 题目原文:
 * 128. 最长连续序列
 *     给定一个未排序的整数数组 nums ，找出数字连续的最长序列（不要求序列元素在原数组中连续）的长度
 *     请你设计并实现时间复杂度为 O(n) 的算法解决此问题。
 *
 * 题目理解:
 *     [个解]1.输入数组 去重排序 从头遍历, 计算最大长度
 *     [官解]2.抓住断点特性 无需排序 只需去重
 *
 * 关键字抽取:
 *     最大连续 => 存在断点 => n - 1 不存在
 *     不要求原数组连续 => Set 结构
 * }}}
 */
object longest_consecutive_sequence {
  private val example: Array[Int] = Array(0, 3, 7, 2, 5, 8, 4, 6, 0, 1)
  private val example_output: Int = 9

  /**
   * {{{
   * 个人思路:
   *     1.题目限制: O(n) 就不允许先排序 再找了, 最快的排序也只能到 n*log_n
   *     2.那就只能 每加入一个新数字 就要重新检验 是否 组成新序列 并判断 是否超越 前一次最长序列长度 ? 这是否也超过了 O(n)?
   *     3.2 太过复杂, 直接有序去重 TreeSet 再 O(n) 遍历计算断点
   *
   * 自我评价: 符合第一直觉 但并不是最优解, 未能抓住 断点特性 -- 前一个数字不存在 => 即 无需排序 可以省掉 TreeSet 排序耗时
   * }}}
   *
   * @param nums 输入数组
   * @return 最大连续长度
   */
  private def longestConsecutive(nums: Array[Int]): Int = {
    // 0.1 初始化
    val ints: mutable.TreeSet[Int] = mutable.TreeSet[Int]()
    val orderedNonRepetitiveList: List[Int] = ints.addAll(nums).toList
    // 0.2 边界条件
    if (orderedNonRepetitiveList.isEmpty) return 0
    // 1.计算最大连续长度
    var latestLength = 0
    var currentLength = 1
    var prev: Int = orderedNonRepetitiveList.head
    orderedNonRepetitiveList.tail.foreach {
      elem =>
        if (prev + 1 == elem) {
          currentLength += 1
        } else {
          // 中部断点
          if (currentLength > latestLength) {
            latestLength = currentLength
          }
          currentLength = 1
        }
        prev = elem
    }
    // 完整无断
    if (currentLength > latestLength) {
      latestLength = currentLength
    }
    latestLength
  }

  /**
   * [官解] 优化版
   *
   * @param nums 输入数组
   * @return 最大连续长度
   */
  private def improved_longestConsecutive(nums: Array[Int]): Int = {
    // 0.初始化
    val values: mutable.Set[Int] = new mutable.HashSet[Int]().addAll(nums)
    var latestLength = 0
    var currentLength = 1

    // 1.遍历 寻找断点并计算长度
    values.foreach {
      elem =>
        // 断点特性: 前一个数字不存在
        if (!values.contains(elem - 1)) {
          var nextValue: Int = elem + 1 
          while (values.contains(nextValue)) {
            currentLength += 1
            nextValue += 1
          }
          if (currentLength > latestLength)
            latestLength = currentLength
          currentLength = 1
        }
    }
    latestLength
  }

  def main(args: Array[String]): Unit = {
    println(longestConsecutive(example))
    println(improved_longestConsecutive(example))
  }
}
