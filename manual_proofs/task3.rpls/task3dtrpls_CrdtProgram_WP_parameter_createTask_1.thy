theory task3dtrpls_CrdtProgram_WP_parameter_createTask_1
imports Why3
begin

why3_open "task3dtrpls_CrdtProgram_WP_parameter_createTask_1.xml"

why3_vc WP_parameter_createTask

proof auto
  show "\<And>assign1 t c1. state_callOps4 assign1 = Op_task_columnid_assign t c1 \<Longrightarrow> False"



why3_end

end
