theory task3dtrpls_CrdtProgram_WP_parameter_moveTask_1
imports Why3
begin

abbreviation undef ("???") where "??? \<equiv> undefined"

why3_open "task3dtrpls_CrdtProgram_WP_parameter_moveTask_1.xml"

why3_vc WP_parameter_moveTask
proof (clarsimp)

  thm H73

  thm H25

  have "state_callOps2 c = state_callOps1 c" if "state_callOps1 c \<noteq> Noop" for c
    by (metis H40 H41 fun_upd_def that)
  hence "state_callOps3 c = state_callOps1 c" if "state_callOps1 c \<noteq> Noop" for c
    by (metis H49 H50 fun_upd_def that)
  hence "state_callOps4 c = state_callOps1 c" if "state_callOps1 c \<noteq> Noop" for c
    by (metis H57 H58 fun_upd_apply that)
  hence state_callOps1_mono:  "state_callOps5 c = state_callOps1 c" if "state_callOps1 c \<noteq> Noop" for c
    by (metis H65 H66 fun_upd_apply that)

  have happensBefore_mono: "state_happensBefore5 c1 c2" if "state_happensBefore1 c1 c2" for c1 c2
    by (smt H24 H39 H40 H41 H42 H43 H49 H50 H51 H52 H57 H59 H60 H63 H65 H67 H68 fun_upd_def select_convs(1) that wellFormed_def)

  have "us2 \<noteq> us3"
    by (metis H58 H65 Select_eq operation.distinct(13))



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
        by (metis H74 state_callOps1_mono happensBefore_mono  operation.simps(49))
    qed
    from this obtain add 
      where add1: "state_callOps1 add = Op_column_tasks_add c1 t"
        and add2: "state_callTransaction1 add = state_callTransaction1 assign1"
        and add3: "(\<nexists>rem. state_callOps1 rem = Op_column_tasks_remove c1 t \<and> state_happensBefore1 rem add = True)"
      by blast


    have "state_callOps5 add = Op_column_tasks_add c1 t"
      by (simp add: add1 state_callOps1_mono)

    find_theorems Op_task_columnid_assign CallId

    
    show  "\<exists>add. state_callOps5 add = Op_column_tasks_add c1 t \<and>
          state_callTransaction1 add = state_callTransaction1 assign1 \<and> (\<forall>rem. state_callOps5 rem = Op_column_tasks_remove c1 t \<longrightarrow> \<not> state_happensBefore5 rem add)"
    proof (cases "column_id = c1 \<and>  task_id = t")
      case True
      thm H74 H66

      from H74
      have H74': "\<not> state_happensBefore5 assign2 assign1" if "c1 \<noteq> c2" and "state_callOps5 assign2 = Op_task_columnid_assign t c2" for assign2 c2
        using that by auto
      have "\<not> state_happensBefore5 (CallId us3) assign1"
      proof (rule H74')
        show "state_callOps5 (CallId us3) = Op_task_columnid_assign t c1"
          by (simp add: H66 True)



      show "\<exists>add. state_callOps5 add = Op_column_tasks_add c1 t \<and>
          state_callTransaction1 add = state_callTransaction1 assign1 \<and> (\<forall>rem. state_callOps5 rem = Op_column_tasks_remove c1 t \<longrightarrow> \<not> state_happensBefore5 rem add)"
      proof (rule exI[where x="CallId us2"], auto)
        show "state_callOps5 (CallId us2) = Op_column_tasks_add c1 t"
          by (auto simp add: H66 H58 True `us2 \<noteq> us3`)
        show "state_callTransaction1 (CallId us2) = state_callTransaction1 assign1"




      hence False


have " \<not> state_happensBefore5 rem add" if "state_callOps5 rem = Op_column_tasks_remove c1 t"  for rem

   
    proof (cases "rem = CallId us1")



    
    proof
      show " state_callOps5 add = Op_column_tasks_add c1 t \<and>
          state_callTransaction1 add = state_callTransaction1 assign1 \<and> (\<forall>rem. state_callOps5 rem = Op_column_tasks_remove c1 t \<longrightarrow> \<not> state_happensBefore5 rem add)"
        apply auto
        apply (simp add: add1 state_callOps1_mono)
        apply (simp add: add2)



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
