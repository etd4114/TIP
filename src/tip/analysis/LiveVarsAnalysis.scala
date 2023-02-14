package tip.analysis

import tip.ast._
import tip.lattices._
import tip.ast.AstNodeData.DeclarationData
import tip.solvers._
import tip.cfg._

import scala.collection.immutable.Set

/**
  * Base class for live variables analysis.
  */
abstract class LiveVarsAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis(false) {

  import tip.cfg.CfgOps._
  import tip.ast.AstOps._

  val lattice: MapLattice[CfgNode, PowersetLattice[ADeclaration]] = new MapLattice(new PowersetLattice())

  val domain: Set[CfgNode] = cfg.nodes

  NoPointers.assertContainsProgram(cfg.prog)
  NoRecords.assertContainsProgram(cfg.prog)

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
    n match {
      case _: CfgFunExitNode => lattice.sublattice.bottom
      case r: CfgStmtNode =>
        r.data match {
          case cond: AExpr => s union cond.appearingIds
          case as: AAssignStmt => 
            as.left match {
              case id: AIdentifier => 
                (s -- id.appearingIds) union as.right.appearingIds 
              case _ => ???
            }
          case varr: AVarStmt => s -- varr.appearingIds
          case ret: AReturnStmt => s
          case out: AOutputStmt => s union out.appearingIds
          case _ => s
        }
      case _ => s
    }


}

/**
  * Live variables analysis that uses the simple fixpoint solver.
  */
class LiveVarsAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends LiveVarsAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with BackwardDependencies

/**
  * Live variables analysis that uses the worklist solver.
  */
class LiveVarsAnalysisWorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends LiveVarsAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with BackwardDependencies
