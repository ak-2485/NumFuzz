open Syntax

let m_zero   = 0.0
let m_one    = 1.0
let si_zero = SiConst m_zero
let si_one  = SiConst m_one

module Simpl = struct
	let max c1 c2 = if c1 <= c2 then c2 else c1

	let rec si_simpl_compute (sis : si) = match sis with
		| SiAdd (si1, si2) ->
      let si1' = si_simpl_compute si1 in
      let si2' = si_simpl_compute si2 in
				begin match si1', si2' with
					| SiConst si1'', SiConst si2'' -> SiConst (si1'' +. si2'')
					| _, SiInfty -> SiInfty
					| SiInfty, _ -> SiInfty
					| _, _ -> sis
				end
		| SiMult (si1, si2) ->
      let si1' = si_simpl_compute si1 in
      let si2' = si_simpl_compute si2 in
				begin match si1', si2' with
					| SiConst si1'', SiConst si2'' -> SiConst (si1'' *. si2'')
					| _, SiInfty -> SiInfty
					| SiInfty, _ -> SiInfty
					| _, _ -> sis
				end
	 | SiDiv (si1, si2) ->
      let si1' = si_simpl_compute si1 in
      let si2' = si_simpl_compute si2 in
				begin match si1', si2' with 
					| SiConst si1'', SiConst si2'' -> SiConst (si1'' /. si2'')
					| SiInfty, SiInfty -> si_one
					| SiConst _, SiInfty -> si_zero
					| _, _ -> sis
				end
	 | SiLub (si1, si2) ->
      let si1' = si_simpl_compute si1 in
      let si2' = si_simpl_compute si2 in
				begin match si1', si2' with
				 | SiConst si1'', SiConst si2'' -> SiConst (max si1'' si2'')
				 | _, SiInfty -> SiInfty
				 | SiInfty, _ -> SiInfty
				 | _, _ -> sis
			end
	 | _ -> sis
end
