package com.inbreeze
package top_100_liked

import scala.collection.mutable

/**
 * {{{
 * Link: https://leetcode.cn/problems/3sum/description/?envType=study-plan-v2&envId=top-100-liked
 *
 * 题目原文:
 * 15. 三数之和
 *     给你一个整数数组 nums ，判断是否存在三元组 [nums[i], nums[j], nums[k]] 满足 i != j、i != k 且 j != k
 *     同时还满足 nums[i] + nums[j] + nums[k] == 0 。请你返回所有和为 0 且不重复的三元组。
 *
 * 题目理解:
 *     [直觉]1.等式变换 负的两数之和 = 另一个数. 直接双 for 两数求和 判断 Map 中是否包含, 包含则余量 -1 [ 拿两个数 找结果 ]
 *            逻辑缺陷: 双 for 无法删除/跳过 已配对三元组的元素 -- 重点: 三个元素 都要跳过
 *     [思路]2.输入数组 -> 单端队列 + HashMap, 循环直到 队列元素 < 3
 *            逻辑问题: 队列 只能顺序 pop 2 个, 还有第三个 在队中, 需要排除
 *                    新增一个 存储待删除第三元素 Map 对比后 消除 避免遍历队列进行修改
 *            ...
 *            -1,0,1,2,-1,-4 => 不要求删除... 没有仔细读题
 *            双端不对 反而 双 for 对了, 只需要去重即可 元素可以重用 只是结果不能重复
 *     [官解] [拿结果 找两个数]
 *
 * 关键字抽取:
 *
 * 重点思维: 默认是 无序环境, 当通过 Sort 变换至 有序环境 后 是否会有更优解法 && 仔细读题
 * }}}
 */
object three_sum {
  private val example: Array[Int] = Array(-2, -3, 0, 0, -2)
  private val example_output: Seq[Seq[Int]] = Seq(Seq(-1, -1, 2), Seq(-1, 0, 1))

  /**
   * <p>耗时长 7.69%, 内存消耗不低 53.75% 
   * <p>坑点也不少
   * <ul>
   * <li>1.for yield 缺陷, 虽然语法简洁, 但会缓存巨大中间集合
   * <li>2.三数同 || 两数同 需要独立判断 + 题目没仔细看
   * </ul>
   * <p>整个算法 整体上 虽然思路简单, 但整体不佳
   *
   * @param nums 输入数组
   * @return
   */
  def threeSum(nums: Array[Int]): List[List[Int]] = {
    // 0.边界判断
    if (nums.length < 3) {
      return List.empty
    } else if (nums.length == 3) {
      if (nums(0) + nums(1) + nums(2) == 0) {
        return List(nums.toList)
      } else {
        return List.empty
      }
    }

    // 1.将原始数组 group_by + count [必须是 Map, K->Cnt 从而避免 自己+自己+第二个数]
    val keyWithCount: Map[Int, Int] = nums.groupMapReduce(identity)(_ => 1)(_ + _)

    // 2. 双 for 遍历 [yield 存在巨大问题 -- 缓存巨大的 中间集合 当循环结束后才会创建 HashSet]
    // 2.1 直接构建结果集，避免 for-yield 产生中间集合
    val resultSet: mutable.Set[List[Int]] = mutable.HashSet[List[Int]]()
    // 2.2 遍历
    for (i <- nums.indices; j <- (i + 1) until nums.length) {
      val num1: Int = nums(i)
      val num2: Int = nums(j)
      val target: Int = -(num1 + num2)

      if (keyWithCount.contains(target)) {
        var isValid = false
        if (target != num1 && target != num2) {
          // Case 1: 三数均不同
          isValid = true
        } else if (target == num1 && target == num2) {
          // Case 2: 三数同 target.cnt >= 3
          if (keyWithCount(target) >= 3) {
            isValid = true
          }
        } else {
          // Case 3: 两数同 target.cnt >= 2
          if (keyWithCount(target) >= 2) {
            isValid = true
          }
        }

        if (isValid) {
          resultSet.add(List(num1, num2, target).sorted)
        }
      }
    }

    resultSet.toList
  }

  /**
   * 官解: 双指针
   * 思路: 1.先排序, 得到升序数组, [-1, 0, 1, 2] -- 用于跳过重复元素
   * 2.查找目标 nums[i] + nums[j] == -nums[k]
   * 归纳: 全为正数/负数 不可能三数为 0, 一定存在 <= 0 的元素 [最特殊为 (0,0,0,0,0)]
   * 其余情况 则为 [-负数 = 负/正数 + 负/正数]
   *
   * @param nums 输入数组
   * @return
   */
  private def improved_threeSum(nums: Array[Int]): List[List[Int]] = {
    // 0.1 边界判断
    if (nums.length < 3) {
      return List.empty
    } else if (nums.length == 3) {
      if (nums(0) + nums(1) + nums(2) == 0) {
        return List(nums.toList)
      } else {
        return List.empty
      }
    }
    // 0.2 构建排序数组
    val sorted: Array[Int] = nums.sorted
    //    println(s"[Print] sorted: ${sorted.mkString("Array(", ", ", ")")}")

    // 0.3 快速反应
    if (sorted.head == sorted.last && sorted.last == 0) {
      // 0.3.1 纯 0 快反
      return List(List(0, 0, 0))
    } else if (sorted.head > 0 || sorted.last < 0) {
      // 0.3.2 纯 正/负 快反
      return List.empty
    } else if (sorted.head == 0 || sorted.last == 0) {
      // 0.3.3 含 0 纯 正 判断 0 数量
      if (sorted.count(_ == 0) >= 3) {
        return List.empty
      } else {
        return List.empty
      }
    }

    // 1.构建结果数组 & 循环
    // ArrayBuilder 高效一次性生成不可变数组 | ArrayBuffer 更加通用
    //    val res: mutable.ArrayBuilder[List[Int]] = mutable.ArrayBuilder.make[List[Int]]()
    val res: mutable.ArrayBuffer[List[Int]] = mutable.ArrayBuffer[List[Int]]()
    // 以最小元素作为 target, 逐步右移
    var targetIndex = 0
    import scala.util.control.Breaks._
    while (targetIndex < nums.length && sorted(targetIndex) <= 0) {
      breakable {
        // 1.1 重复跳过
        if (targetIndex > 0 && sorted(targetIndex) == sorted(targetIndex - 1)) {
          targetIndex += 1
          break()
        }
        //        println(s"[Print] target: ${-sorted(targetIndex)} targetIndex: $targetIndex")
        // 1.2 左右双指针
        var left: Int = targetIndex + 1
        var right: Int = nums.length - 1
        val target: Int = -sorted(targetIndex)

        // 1.3 左右指针相向遍历 直到相遇停止 [是否有更好的提前停止]
        while (left < right) {
          // 1.3.1 判断是否三数和 = 0
          val sum: Int = sorted(left) + sorted(right)
          //          println(s"[Print] sum: $sum = ${sorted(left)} [$left] + ${sorted(right)} [$right]")
          if (sum == target) {
            //            println(s"[Print] hit: ${sorted(targetIndex)}, ${sorted(left)}, ${sorted(right)}")
            res += List(sorted(targetIndex), sorted(left), sorted(right))
            // 1.3.2 l & r 更新
            left += 1
            right -= 1
            // 1.3.3 l & r 跳过重复
            while (left < right && sorted(left) == sorted(left - 1)) left += 1
            while (left < right && sorted(left) == sorted(right + 1)) right -= 1
          } else if (sum < target) {
            left += 1
          } else {
            right -= 1
          }
        }
        targetIndex += 1
      }
    }
    // res.result().toList
    res.toList
  }


  def main(args: Array[String]): Unit = {
    println(improved_threeSum(example))
  }
}
