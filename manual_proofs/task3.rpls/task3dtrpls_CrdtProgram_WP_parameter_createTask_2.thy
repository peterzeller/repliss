theory task3dtrpls_CrdtProgram_WP_parameter_createTask_2
imports Why3
begin

abbreviation undef2 ("???") where "??? \<equiv> undefined"

why3_open "task3dtrpls_CrdtProgram_WP_parameter_createTask_2.xml"

why3_vc WP_parameter_createTask
proof -

  find_theorems "state_callOps1"

  from H71 H63 H55
  have updates: "state_callOps4 = state_callOps1(
            CallId us := Op_column_tasks_add column_id t,
            CallId us1 := Op_task_title_assign t title,
            CallId us2 := Op_task_columnid_assign t column_id
            )"
    by auto

  have [simp]: "us1 \<noteq> us"
    by (metis H55 H62 Select_eq operation.distinct(13))
  have [simp]: "us2 \<noteq> us" 
    by (metis H55 H63 H70 Select_neq \<open>us1 \<noteq> us\<close> callId.inject fun_upd_same operation.distinct(13))
  hence [simp]: "us \<noteq> us2"
    by force
  have [simp]: "us1 \<noteq> us2"
    by (metis H63 H70 fun_upd_def operation.distinct(20))
  hence [simp]: "us2 \<noteq> us1"
    by force


  show "\<nexists>assign1 t1 c1. state_callOps4 assign1 = Op_task_columnid_assign t1 c1"
    apply (auto simp add: H71 H63 H55)


why3_end

end
