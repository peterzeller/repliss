theory task3dtrpls_CrdtProgram_WP_parameter_moveTask_1
imports Why3
begin

why3_open "task3dtrpls_CrdtProgram_WP_parameter_moveTask_1.xml"

why3_vc WP_parameter_moveTask
proof (clarsimp)

  thm H73

  show "\<exists>add. state_callOps5 add = Op_column_tasks_add c1 t 
    \<and> state_callTransaction1 add = state_callTransaction1 assign1 
    \<and> (\<forall>rem. state_callOps5 rem = Op_column_tasks_remove c1 t \<longrightarrow> \<not> state_happensBefore5 rem add)"
  proof (cases "state_callOps1 assign1 = Op_task_columnid_assign t c1")
    case True
    find_theorems Op_column_tasks_remove

    have "\<exists>add. state_callOps1 add = Op_column_tasks_add c1 t \<and>
            state_callTransaction1 add = state_callTransaction1 assign1 \<and>
            (\<nexists>rem. state_callOps1 rem = Op_column_tasks_remove c1 t \<and> state_happensBefore1 rem add = True)"
    proof (rule H25)
      show " state_callOps1 assign1 = Op_task_columnid_assign t c1"
        using True by auto
      show "\<nexists>assign2 c2. c1 \<noteq> c2 \<and> state_callOps1 assign2 = Op_task_columnid_assign t c2 \<and> state_happensBefore1 assign2 assign1 = True"
        using H74
        apply auto
        apply (drule_tac x=assign2 in spec)
        apply (drule_tac x=c2 in spec)
        apply auto



    have "\<exists>add. state_callOps1 add = Op_column_tasks_add c1 t 
    \<and> state_callTransaction1 add = state_callTransaction1 assign1 
    \<and> (\<forall>rem. state_callOps1 rem = Op_column_tasks_remove c1 t \<longrightarrow> \<not> state_happensBefore1 rem add)"
      using H25 
      

    then show ?thesis 

  next
    case False
    then show ?thesis sorry
  qed





why3_end

end
