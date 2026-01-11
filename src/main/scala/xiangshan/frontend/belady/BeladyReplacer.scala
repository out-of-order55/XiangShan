// Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.frontend.belady

import chisel3._
import chisel3.experimental.ExtModule
import chisel3.util._
import freechips.rocketchip.util.UIntToAugmentedUInt
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.HasFrontendParameters
import xiangshan.frontend.bpu.BpuModule

class BeladyReplacer(numWays: Int, numSets: Int) extends ExtModule() with HasExtModuleInline {
  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))
  val io = IO(new Bundle {
    val isEviction = Input(Bool())
    val victimWay  = Output(UInt(log2Ceil(numWays).W))
    // val writeStateValid = Input(Bool())
    val searchPc = Input(Vec(numWays, UInt(64.W)))

  })
  private val beladyDpiLines: Seq[String] = {
    val pcIns = (0 until numWays).map(i => s"  input longint pc${i},")
    val lines = Seq(
      "import \"DPI-C\" function void SimBeladyDistance (",
      "  input bit isEviction,",
      "  output int victimWay,"
    ) ++ pcIns
    lines.updated(lines.size - 1, lines.last.stripSuffix(",") + ");")
  }

  private val idxBits = log2Ceil(numWays)
  val verilogLines = beladyDpiLines ++ {
    val pcPorts = (0 until numWays).map(i => f"  input  [63:0] io_searchPc_${i},")
    Seq(
      s"module BeladyReplacer(",
      "  input         clock,",
      "  input         reset,",
      "  input         io_isEviction,"
    ) ++ pcPorts ++ Seq(
      s"  output [${numWays - 1}:0] io_victimWay",
      ");",
      "",
      "  integer victimWay;",
      s"  reg  [${numWays - 1}:0] victimMask;",
      s"  wire [${idxBits - 1}:0] victimIdx = victimWay[${idxBits - 1}:0];",
      "",
      "  always @(*) begin",
      "    SimBeladyDistance(",
      "      io_isEviction,"
    ) ++ (0 until numWays).map(i => f"      io_searchPc_${i},") :+ "      victimWay"
  } ++ Seq(
    "    );",
    "",
    "  assign io_victimWay = victimIdx;",
    "endmodule"
  )
  setInline(s"$desiredName.v", verilogLines.mkString("\n"))
}
class BeladyReplacerModule(numWays: Int, numSets: Int, numBanks: Int)(implicit p: Parameters)
    extends BpuModule {
  val io = IO(new Bundle {
    val isEviction    = Input(Bool())
    val victimWay     = Output(UInt(log2Ceil(numWays).W))
    val victimSetIdx  = Input(UInt(log2Ceil(numSets).W))
    val victimBankIdx = Input(UInt(log2Ceil(numBanks).W))
//    val writeStateValid = Input(Vec(numBanks,Bool()))
    val writeBankMask = Input(UInt(numBanks.W))
    val writeWayMask  = Input(Vec(numBanks, UInt(numWays.W)))
    val writeSetIdx   = Input(Vec(numBanks, Vec(numWays, UInt(log2Ceil(numSets).W))))
    val writePc       = Input(Vec(numBanks, Vec(numWays, UInt(VAddrBits.W))))

  })
  val debugPcReg =
    RegInit(VecInit(Seq.fill(numBanks)(VecInit(Seq.fill(numSets)(VecInit(Seq.fill(numWays)(0.U(64.W))))))))

  for (i <- 0 until numBanks) {
    for (j <- 0 until numWays) {
      when(io.writeBankMask(i).asBool && io.writeWayMask(i)(j).asBool) {
        debugPcReg(i)(io.writeSetIdx(i)(j))(j) := io.writePc(i)(j)
      }
    }
  }

  val belady = Module(new BeladyReplacer(numWays, numSets))
  belady.clock         := this.clock
  belady.reset         := this.reset
  belady.io.isEviction := io.isEviction
  belady.io.searchPc   := debugPcReg(io.victimBankIdx)(io.victimSetIdx)
  io.victimWay         := belady.io.victimWay
}
