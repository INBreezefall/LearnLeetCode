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
  private val example: Array[Int] = Array(-1, 0, 1, 0)
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
   * 读错题实现: 不重用元素 输出 和为零的三元组 [单端队列]
   *
   * @param nums 输入数组
   * @return
   */
  def threeSumWithNonRepetitiveElements(nums: Array[Int]): List[List[Int]] = {
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

    // 1.1 单端队列准备
    val queue: mutable.Queue[Int] = mutable.Queue(nums *)
    // 1.2 K-Cnt Map 准备
    val keyWithCount: mutable.Map[Int, Int] = nums
      .groupBy(i => i)
      .map((k, arr) => (k, arr.length))
      .to(mutable.Map)
    // 1.3 待跳过的 队中第三元素 Map
    val thirdElemMap: mutable.Map[Int, Int] = mutable.HashMap[Int, Int]()

    // 1.4 cnt - 1 函数定义
    def decCountFromSpecialMap(key: Int, map: mutable.Map[Int, Int]): Unit = {
      println(s"decCount $key")
      map.updateWith(key) {
        // Count - 1
        case Some(c) if c >= 2 => println(s"$key.count = $c - 1"); Some(c - 1)
        // 删除映射
        case Some(c) if c == 1 => println(s"$key == 0"); None
        // 不应该走到的地方
        case _: Option[Int] => throw new Exception("Mistakes that should not occur")
      }
    }

    // 1.5 cnt + 1 函数定义
    def incCountFromSpecialMap(key: Int, map: mutable.Map[Int, Int]): Unit = {
      println(s"incCount $key")
      map.updateWith(key) {
        // None + 1
        case None => println(s"$key None + 1 = 1"); Some(1)
        // c + 1
        case Some(c) => println(s"$key.count = $c + 1"); Some(c + 1)
        // 不应该走到的地方
        case _: Option[Int] => throw new Exception("Mistakes that should not occur")
      }
    }

    // 1.6 获取队首元素 碰撞 thirdElemMap 消除
    def dequeueWithCrash(): Int = {
      var v: Int = queue.dequeue()
      if (thirdElemMap.contains(v)) {
        println(s"dequeue crash $v")
        decCountFromSpecialMap(v, thirdElemMap)
        v = dequeueWithCrash()
      } else {
        println(s"dequeue didn't crash $v")
      }
      v
    }

    // 2.循环 直到 队列元素 < 3
    val result: mutable.ArrayBuffer[List[Int]] = mutable.ArrayBuffer[List[Int]]()
    while (queue.size >= 3) {
      val i: Int = dequeueWithCrash()
      val j: Int = dequeueWithCrash()
      val k: Int = -i - j
      Seq(i, j).foreach(e => decCountFromSpecialMap(e, keyWithCount))
      if (keyWithCount.contains(k)) {
        println(s"hit the target: $i, $j, $k")
        result.addOne(List(i, j, k))
        decCountFromSpecialMap(k, keyWithCount)
        incCountFromSpecialMap(k, thirdElemMap)
      }
    }

    // 3.返回结果
    result.toList
  }


  def main(args: Array[String]): Unit = {
    println(threeSum(example))
  }
}
