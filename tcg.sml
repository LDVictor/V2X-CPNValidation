(* implementation of the test case specification for the TPC example *)

structure TPCTCSpec : TCSPEC = struct

fun detection (Bind.Comportamentos_de_V'Verifica_se_V_esta_ou_nao_em_cobertura _)  = true
  | detection (Bind.Autenticacao_fora_de_cobertura'Verifica_credencial_de_V' _)  = true
  | detection (Bind.Proof_of_Reputation'Inicia_o_Proof_of_Reputation _)  = true
  | detection (Bind.Proof_of_Reputation'Verifica_se_o_IC_foi_valido _)  = true
  | detection (Bind.Autenticacao_em_cobertura'RAN_checa_credencial_no_VDR _)  = true
  | detection (Bind.RAN'RAN_envia_o_conteudo_da_mensagem_ao_servidor_ADAS _)  = true
  | detection _ = false;

exception obsExn;
fun observation (Bind.Comportamentos_de_V'Verifica_se_V_esta_ou_nao_em_cobertura (_, {id, v_msg})) = [InEvent (IDxV_MSG(id, v_msg))]
  | observation (Bind.Autenticacao_fora_de_cobertura'Verifica_credencial_de_V' (_, {v_msg})) = [OutEvent (V_MSG(v_msg))]
  | observation (Bind.Proof_of_Reputation'Inicia_o_Proof_of_Reputation (_, {v_msg})) = [InEvent (V_MSG(v_msg))]
  | observation (Bind.Proof_of_Reputation'Verifica_se_o_IC_foi_valido (_, {ic})) = [OutEvent (V_MSG(v_msg))]
  | observation (Bind.Autenticacao_em_cobertura'RAN_checa_credencial_no_VDR (_, {v_msg})) = [OutEvent (V_MSG(v_msg))]  
  | observation (Bind.RAN'RAN_envia_o_conteudo_da_mensagem_ao_servidor_ADAS (_, {v_msg})) = [OutEvent (MSG(msg))]  
  | observation _ = raise obsExn; 

fun format (InEvent (IDxV_MSG(id, 
v_msg))) =
  "      <Standard>\n"^
  "        <Insulin>^c^</Insulin>\n"^
  "      </Standard>\n"
  | format (OutEvent (MSG(msg))) =
    "        <Decision>\n"^
    "          <DecisionValue></DecisionValue>\n"^
    "        </Decision>\n";
end;

(* setup test case generation for the TPC example *)
Config.setTCdetect(TPCTCSpec.detection);
Config.setTCobserve(TPCTCSpec.observation);
Config.setTCformat(TPCTCSpec.format);

(* logging and output *)
Config.setModelDir (mbtcpnlibpath^"examples/v2x-cpn/");
Config.setOutputDir ((Config.getModelDir())^"tests/");

(* configuration and test case naming *)
Config.setConfigNaming (fn () => "tiotatc");
Config.setTCNaming(fn i => "CaseID=\""^(Int.toString i)^"\" NumOf=\"");
