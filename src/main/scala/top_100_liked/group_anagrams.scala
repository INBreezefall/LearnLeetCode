package com.inbreeze
package top_100_liked

import scala.collection.mutable

/**
 * {{{
 * Link: https://leetcode.cn/problems/group-anagrams/?envType=study-plan-v2&envId=top-100-liked
 * 
 * 题目原文:
 * 49. 字母异位词分组
 *     给你一个字符串数组，请你将 字母异位词 组合在一起
 *     可以按任意顺序返回结果列表
 *     字母异位词 是由 重新排列 源单词的所有字母得到的一个新单词
 *
 * 题目理解:
 *     具有相同字母组成的单词 分到同一组, 组间无序即可
 * 
 * 关键字抽取:
 *     分组 => Map 结构
 * }}}
 */
object group_anagrams {
  private val example: Array[String] = Array("eat", "tea", "tan", "ate", "nat", "bat")
  private val example_output: Seq[Seq[String]] = Seq(Seq("bat"), Seq("nat", "tan"), Seq("ate", "eat", "tea"))

  /**
   * {{{
   * 个人思路:
   * 分组 -> HashMap
   *      -> Map[Key: String -> Value: Seq]
   *      Key = 单词内部按字母序重排后的变换值(唯一值)
   *      Value = 原始单词值
   *}}}
   * @param strs 数据输入
   * @return
   */
  def groupAnagrams(strs: Array[String]): List[List[String]] = {
    val resMap: mutable.Map[String, mutable.ArrayBuffer[String]] = new mutable.HashMap(100, 0.75)
    strs.foreach {
      str =>
        val sortedKey: String = str.sorted
        resMap.put(sortedKey, resMap.getOrElse(sortedKey, mutable.ArrayBuffer()).append(str))
    }
    resMap.values.map(_.toList).toList
  }

  @main def main(): Unit = {
    groupAnagrams(example)
  }
}

