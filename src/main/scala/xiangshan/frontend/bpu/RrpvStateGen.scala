// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

// The design of this file is based on the implementation of PseudoLRU in rock-chip at:
//          https://github.com/chipsalliance/rocket-chip/blob/master/src/main/scala/util/Replacement.scala
// See LICENSE.Berkeley for license details at:
//          https://github.com/chipsalliance/rocket-chip/blob/master/LICENSE.Berkeley
// See LICENSE.SiFive for license details at:
//          https://github.com/chipsalliance/rocket-chip/blob/master/LICENSE.SiFive

package xiangshan.frontend.bpu

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.UIntToAugmentedUInt

class RrpvStateGen(val n_ways: Int, val accessSize: Int = 1,val rrpvBits: Int) extends Module {

  class RrpvStateGenIO extends Bundle {
    val stateIn:    Vec[UInt]             = Input(Vec(n_ways, UInt(rrpvBits.W)))
    val touchWays:  Vec[Valid[UInt]] = Input(Vec(accessSize, Valid(UInt(log2Ceil(n_ways).W))))
    val nextState:  Vec[UInt]             = Output(Vec(n_ways, UInt(rrpvBits.W)))
		val isEviction: Bool             = Input(Bool())
    val replaceWay: UInt             = Output(UInt(log2Ceil(n_ways).W))
  }
  val io: RrpvStateGenIO = IO(new RrpvStateGenIO)
	// if a way is touched, set its RRPV to 0
  def getNextState(state: Vec[UInt], touchWays: Seq[Valid[UInt]]): Vec[UInt] ={
		val nextState = WireInit(state)

		for(i <- 0 until accessSize) {
			val isTouched = touchWays.map{ touchWay =>
				touchWay.valid && (touchWay.bits === i.U)
			}.reduce(_ || _)
			when(isTouched) {
				nextState(i) := Mux(state(i)===0.U,0.U(rrpvBits.W),state(i)-1.U)
			}.otherwise{
				nextState(i) := state(i)
			}
		}
		nextState
  }
	val maxRrpv = ((1<<rrpvBits)-1).U(rrpvBits.W)
	// get the way with max RRPV
  def getReplaceWay(state: Vec[UInt], Nways: Int): UInt = {
    
    val distances = state.map(s => (maxRrpv-s))
		val wayCompareMatrix = CompareMatrix(VecInit(distances))
		//find the min distance way
		val minDistanceMask =(0 until n_ways).map{i=>
			PopCount(wayCompareMatrix(i))===(n_ways-1).U
		}
		PriorityEncoder(minDistanceMask)
  }

  def getReplaceWay(state: Vec[UInt]): UInt = getReplaceWay(state, n_ways)

  def way(state: Vec[UInt]): UInt = getReplaceWay(state)

  io.replaceWay := getReplaceWay(io.stateIn)

  protected val stateOut = WireInit(VecInit(Seq.fill(n_ways)((1<<rrpvBits-2).U(rrpvBits.W))))

  def access(touchWays: Seq[Valid[UInt]]): Unit =
    when(touchWays.map(_.valid).reduce(_ || _)) {
      stateOut := getNextState(io.stateIn, touchWays)
  }
	//update RRPV during eviction
	def aging(victimWay: UInt):	Unit = {
		val distance = maxRrpv - io.stateIn(victimWay)
		for(i <- 0 until n_ways) {
			when(i.U === victimWay){
				stateOut(i) := ((1<<rrpvBits)-2).U(rrpvBits.W)
			}.otherwise{
				stateOut(i) := io.stateIn(i) + distance
			}
		}
	}
	when(io.isEviction){
		aging(io.replaceWay)
	}.otherwise{
		access(io.touchWays.toSeq)
	}
  io.nextState := stateOut
}

