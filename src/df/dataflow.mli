module type AnalysisType = sig
  type t

  val print : Format.formatter -> t -> unit
  val algo : Cfg.cfg -> t
  val get_result : t -> (Types.lbl * (Types.lbl * (Domain.result * Domain.result)) list) list
end

module ForwardAnalysis (D : Domain.Domain) : AnalysisType
module BackwardAnalysis (D : Domain.Domain) : AnalysisType
