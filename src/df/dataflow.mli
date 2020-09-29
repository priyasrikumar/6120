module type AnalysisType = sig
  type t

  val print : Format.formatter -> t -> unit
  val algo : Cfg.blocks_t -> Cfg.cfg_t -> Cfg.cfg_t -> t
end

module ForwardAnalysis (D : Domain.Domain) : AnalysisType
module BackwardAnalysis (D : Domain.Domain) : AnalysisType