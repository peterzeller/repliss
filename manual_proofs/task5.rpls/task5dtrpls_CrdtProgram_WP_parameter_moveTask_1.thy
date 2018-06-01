theory task5dtrpls_CrdtProgram_WP_parameter_moveTask_1
  imports Why3
begin

why3_open "task5dtrpls_CrdtProgram_WP_parameter_moveTask_1.xml"



why3_vc WP_parameter_moveTask
proof (auto simp add: column_tasks_contains_def)

  have us_distinct: "distinct [us, us1, us2, us3]"
    by (smt H41 H43 H46 H48 H49 H51 H54 H56 H57 H64 distinct_length_2_or_more distinct_one fun_upd_same fun_upd_twist operation.distinct(13) select_convs(1) wellFormed_def)
  hence [simp]: "us \<noteq> us1" "us \<noteq> us2" "us \<noteq> us3"
    "us1 \<noteq> us" "us1 \<noteq> us2" "us1 \<noteq> us3"
    "us2 \<noteq> us" "us2 \<noteq> us1" "us2 \<noteq> us3"
    "us3 \<noteq> us" "us3 \<noteq> us1" "us3 \<noteq> us2"
    by auto


  find_theorems column_tasks_contains
    (* invariant holds in pre-state *)
  from H26 have inv_pre: "\<forall>c1 t. task_columnid_isEqualTo t c1 \<lparr>contents = state_locallyGenerated_taskId\<rparr> \<lparr>contents = state_exposed_taskId\<rparr> \<lparr>contents = state_knownIds_taskId\<rparr>
            \<lparr>contents = state_locallyGenerated_columnId\<rparr> \<lparr>contents = state_exposed_columnId\<rparr> \<lparr>contents = state_knownIds_columnId\<rparr> \<lparr>contents = state_locallyGenerated_boardId\<rparr>
            \<lparr>contents = state_exposed_boardId\<rparr> \<lparr>contents = state_knownIds_boardId\<rparr> \<lparr>contents = state_callOps1\<rparr> \<lparr>contents = state_visibleCalls1\<rparr>
            \<lparr>contents = state_happensBefore1\<rparr> \<lparr>contents = state_callTransaction1\<rparr> \<lparr>contents = state_currentTransaction1\<rparr>
            \<lparr>contents = TransactionId state_currentTransactionId1\<rparr> \<lparr>contents = state_origin1\<rparr> \<lparr>contents = state_invocations2\<rparr> \<lparr>contents = state_invocationResult1\<rparr>
            \<lparr>contents = state_invocationHappensBefore1\<rparr> \<longrightarrow>
           column_tasks_contains c1 t \<lparr>contents = state_locallyGenerated_taskId\<rparr> \<lparr>contents = state_exposed_taskId\<rparr> \<lparr>contents = state_knownIds_taskId\<rparr>
            \<lparr>contents = state_locallyGenerated_columnId\<rparr> \<lparr>contents = state_exposed_columnId\<rparr> \<lparr>contents = state_knownIds_columnId\<rparr> \<lparr>contents = state_locallyGenerated_boardId\<rparr>
            \<lparr>contents = state_exposed_boardId\<rparr> \<lparr>contents = state_knownIds_boardId\<rparr> \<lparr>contents = state_callOps1\<rparr> \<lparr>contents = state_visibleCalls1\<rparr>
            \<lparr>contents = state_happensBefore1\<rparr> \<lparr>contents = state_callTransaction1\<rparr> \<lparr>contents = state_currentTransaction1\<rparr>
            \<lparr>contents = TransactionId state_currentTransactionId1\<rparr> \<lparr>contents = state_origin1\<rparr> \<lparr>contents = state_invocations2\<rparr> \<lparr>contents = state_invocationResult1\<rparr>
            \<lparr>contents = state_invocationHappensBefore1\<rparr>"
    by auto

(*
  thm H62
  from H62 
  have 
wf1: "\<forall>c1 c2. contents \<lparr>contents = state_callOps4\<rparr> c1 = Noop \<longrightarrow> contents \<lparr>contents = state_happensBefore4\<rparr> c2 c1 \<noteq> True"
     and wf1: "\<forall>c1 c2. contents \<lparr>contents = state_callOps4\<rparr> c2 = Noop \<longrightarrow> contents \<lparr>contents = state_happensBefore4\<rparr> c2 c1 \<noteq> True"
     and wf2: "\<forall>c. contents \<lparr>contents = state_visibleCalls4\<rparr> c = True \<longrightarrow> contents \<lparr>contents = state_callOps4\<rparr> c \<noteq> Noop"
     and wf3: "\<forall>c1 c2. contents \<lparr>contents = state_visibleCalls4\<rparr> c1 = True \<and>
             contents \<lparr>contents = state_callTransaction1\<rparr> c1 = contents \<lparr>contents = state_callTransaction1\<rparr> c2 \<and> contents \<lparr>contents = state_callOps4\<rparr> c2 \<noteq> Noop \<longrightarrow>
             contents \<lparr>contents = state_visibleCalls4\<rparr> c2 = True"
     and wf4: "\<forall>c1 c2. contents \<lparr>contents = state_visibleCalls4\<rparr> c2 = True \<and> contents \<lparr>contents = state_happensBefore4\<rparr> c2 c1 = True \<longrightarrow>
             contents \<lparr>contents = state_visibleCalls4\<rparr> c1 = True"
     and wf5: "\<forall>c. contents \<lparr>contents = state_callOps4\<rparr> c \<noteq> Noop \<longrightarrow> contents \<lparr>contents = state_happensBefore4\<rparr> c c = True"
     and wf6: "\<forall>x y z. contents \<lparr>contents = state_happensBefore4\<rparr> y x = True \<and> contents \<lparr>contents = state_happensBefore4\<rparr> z y = True \<longrightarrow>
             contents \<lparr>contents = state_happensBefore4\<rparr> z x = True"
     and wf7: "\<forall>x y. contents \<lparr>contents = state_happensBefore4\<rparr> y x = True \<and> contents \<lparr>contents = state_happensBefore4\<rparr> x y = True \<longrightarrow> x = y"
     and wf8: "\<forall>c1 c2. contents \<lparr>contents = state_callOps4\<rparr> c1 \<noteq> Noop \<and>
             contents \<lparr>contents = state_callOps4\<rparr> c2 \<noteq> Noop \<and>
             contents \<lparr>contents = state_invocationHappensBefore1\<rparr> (contents \<lparr>contents = state_origin4\<rparr> c1, contents \<lparr>contents = state_origin4\<rparr> c2) = True \<longrightarrow>
             contents \<lparr>contents = state_happensBefore4\<rparr> c2 c1 = True"
     and wf9: "\<forall>i. contents \<lparr>contents = state_invocations2\<rparr> i = NoInvocation \<longrightarrow> contents \<lparr>contents = state_invocationResult1\<rparr> i = NoResult"
     and wf10: "\<forall>i1 i2. contents \<lparr>contents = state_invocationResult1\<rparr> i1 = NoResult \<longrightarrow> contents \<lparr>contents = state_invocationHappensBefore1\<rparr> (i1, i2) \<noteq> True"
     and wf11: "\<forall>i1 i2. contents \<lparr>contents = state_invocationResult1\<rparr> i1 = NoResult \<longrightarrow> contents \<lparr>contents = state_invocationHappensBefore1\<rparr> (i2, i1) \<noteq> True"
     and wf12: "\<forall>i1 i2. contents \<lparr>contents = state_invocationHappensBefore1\<rparr> (i1, i2) = True \<longrightarrow> contents \<lparr>contents = state_invocationResult1\<rparr> i1 \<noteq> NoResult"
     and wf13: "\<forall>i1 i2. contents \<lparr>contents = state_invocationHappensBefore1\<rparr> (i1, i2) = True \<longrightarrow> contents \<lparr>contents = state_invocationResult1\<rparr> i2 \<noteq> NoResult"
     and wf14: "\<forall>x1 x2 y1 y2.
        contents \<lparr>contents = state_callTransaction1\<rparr> x1 = contents \<lparr>contents = state_callTransaction1\<rparr> x2 \<and>
        contents \<lparr>contents = state_callTransaction1\<rparr> y1 = contents \<lparr>contents = state_callTransaction1\<rparr> y2 \<and>
        contents \<lparr>contents = state_callTransaction1\<rparr> x1 \<noteq> contents \<lparr>contents = state_callTransaction1\<rparr> y1 \<and> contents \<lparr>contents = state_happensBefore4\<rparr> y1 x1 = True \<longrightarrow>
        contents \<lparr>contents = state_happensBefore4\<rparr> y2 x2 = True"
    by (unfold wellFormed_def, metis)+
*)
  from H54
  have 
    wf1: "\<forall>c1 c2. contents \<lparr>contents = state_callOps3\<rparr> c1 = Noop \<longrightarrow> contents \<lparr>contents = state_happensBefore3\<rparr> c2 c1 \<noteq> True"
    and wf1: "\<forall>c1 c2. contents \<lparr>contents = state_callOps3\<rparr> c2 = Noop \<longrightarrow> contents \<lparr>contents = state_happensBefore3\<rparr> c2 c1 \<noteq> True"
    and wf2: "\<forall>c. contents \<lparr>contents = state_visibleCalls3\<rparr> c = True \<longrightarrow> contents \<lparr>contents = state_callOps3\<rparr> c \<noteq> Noop"
    and wf3: "\<forall>c1 c2. contents \<lparr>contents = state_visibleCalls3\<rparr> c1 = True \<and>
             contents \<lparr>contents = state_callTransaction1\<rparr> c1 = contents \<lparr>contents = state_callTransaction1\<rparr> c2 \<and> contents \<lparr>contents = state_callOps3\<rparr> c2 \<noteq> Noop \<longrightarrow>
             contents \<lparr>contents = state_visibleCalls3\<rparr> c2 = True"
    and wf4: "\<forall>c1 c2. contents \<lparr>contents = state_visibleCalls3\<rparr> c2 = True \<and> contents \<lparr>contents = state_happensBefore3\<rparr> c2 c1 = True \<longrightarrow>
             contents \<lparr>contents = state_visibleCalls3\<rparr> c1 = True"
    and wf5: "\<forall>c. contents \<lparr>contents = state_callOps3\<rparr> c \<noteq> Noop \<longrightarrow> contents \<lparr>contents = state_happensBefore3\<rparr> c c = True"
    and wf6: "\<forall>x y z. contents \<lparr>contents = state_happensBefore3\<rparr> y x = True \<and> contents \<lparr>contents = state_happensBefore3\<rparr> z y = True \<longrightarrow>
             contents \<lparr>contents = state_happensBefore3\<rparr> z x = True"
    and wf7: "\<forall>x y. contents \<lparr>contents = state_happensBefore3\<rparr> y x = True \<and> contents \<lparr>contents = state_happensBefore3\<rparr> x y = True \<longrightarrow> x = y"
    and wf8: "\<forall>c1 c2. contents \<lparr>contents = state_callOps3\<rparr> c1 \<noteq> Noop \<and>
             contents \<lparr>contents = state_callOps3\<rparr> c2 \<noteq> Noop \<and>
             contents \<lparr>contents = state_invocationHappensBefore1\<rparr> (contents \<lparr>contents = state_origin3\<rparr> c1, contents \<lparr>contents = state_origin3\<rparr> c2) = True \<longrightarrow>
             contents \<lparr>contents = state_happensBefore3\<rparr> c2 c1 = True"
    and wf9: "\<forall>i. contents \<lparr>contents = state_invocations2\<rparr> i = NoInvocation \<longrightarrow> contents \<lparr>contents = state_invocationResult1\<rparr> i = NoResult"
    and wf10: "\<forall>i1 i2. contents \<lparr>contents = state_invocationResult1\<rparr> i1 = NoResult \<longrightarrow> contents \<lparr>contents = state_invocationHappensBefore1\<rparr> (i1, i2) \<noteq> True"
    and wf11: "\<forall>i1 i2. contents \<lparr>contents = state_invocationResult1\<rparr> i1 = NoResult \<longrightarrow> contents \<lparr>contents = state_invocationHappensBefore1\<rparr> (i2, i1) \<noteq> True"
    and wf12: "\<forall>i1 i2. contents \<lparr>contents = state_invocationHappensBefore1\<rparr> (i1, i2) = True \<longrightarrow> contents \<lparr>contents = state_invocationResult1\<rparr> i1 \<noteq> NoResult"
    and wf13: "\<forall>i1 i2. contents \<lparr>contents = state_invocationHappensBefore1\<rparr> (i1, i2) = True \<longrightarrow> contents \<lparr>contents = state_invocationResult1\<rparr> i2 \<noteq> NoResult"
    and wf13: "\<forall>x1 x2 y1 y2.
        contents \<lparr>contents = state_callTransaction1\<rparr> x1 = contents \<lparr>contents = state_callTransaction1\<rparr> x2 \<and>
        contents \<lparr>contents = state_callTransaction1\<rparr> y1 = contents \<lparr>contents = state_callTransaction1\<rparr> y2 \<and>
        contents \<lparr>contents = state_callTransaction1\<rparr> x1 \<noteq> contents \<lparr>contents = state_callTransaction1\<rparr> y1 \<and> contents \<lparr>contents = state_happensBefore3\<rparr> y1 x1 = True \<longrightarrow>
        contents \<lparr>contents = state_happensBefore3\<rparr> y2 x2 = True"
    by (unfold wellFormed_def, metis)+

  have wf_hb_antisym3: "\<forall>x y. state_happensBefore3 y x \<and> state_happensBefore3 x y \<longrightarrow> x = y"
    by (smt H54 select_convs(1) wellFormed_def)


  have wf_hb_antisym4: "\<forall>x y. state_happensBefore4 y x \<and> state_happensBefore4 x y \<longrightarrow> x = y"
    by (smt H54 H56 H58 Select_neq select_convs(1) wellFormed_def)

  have wf_hb_antisym5: "\<forall>x y. state_happensBefore5 y x \<and> state_happensBefore5 x y \<longrightarrow> x = y"
    by (smt H70 select_convs(1) wellFormed_def)



  show "\<exists>add. state_visibleCalls5 add \<and>
          state_callOps5 add = Op_column_tasks_add c1 t \<and>
          (\<forall>rem. state_callOps5 rem = Op_column_tasks_remove c1 t \<longrightarrow> state_visibleCalls5 rem \<longrightarrow> \<not> state_happensBefore5 rem add)" (is ?goal)
  proof (cases "c1 = column_id \<and> t = task_id ")
    case True
    text "The task that has been moved"
    hence "c1 = column_id" and "t = task_id" by auto

    show ?goal
    proof (rule exI[where x="CallId us2"], auto)
      show "state_visibleCalls5 (CallId us2)"
        by (simp add: H59 H67)
      show "state_callOps5 (CallId us2) = Op_column_tasks_add c1 t"
        using H57 H64 H65 True by auto
      show "\<And>rem. \<lbrakk>state_callOps5 rem = Op_column_tasks_remove c1 t; state_visibleCalls5 rem; state_happensBefore5 rem (CallId us2)\<rbrakk> \<Longrightarrow> False"
        apply (auto simp add: H57 H58 H65 H66 H67 H49 H59 H51 H43 H50 H42 H41 split: if_splits)
        using H43 H51 H56 wf2 apply auto[1]
        using H27 H43 H51 H56 wf2 by auto

    qed
  next
    case False

    text "Register t.columnId  has value c1, so there must be corresponding database calls:"
    from H72
    obtain assign1 
      where a1: "state_visibleCalls5 assign1"
        and a2: "state_callOps5 assign1 = Op_task_columnid_assign t c1"
        and a3: "\<forall>assign2. state_visibleCalls5 assign2 \<and> assign1 \<noteq> assign2 \<and> (\<exists>value. state_callOps5 assign2 = Op_task_columnid_assign t value) \<longrightarrow> \<not> state_happensBefore5 assign2 assign1"
      by (auto simp add:  task_columnid_isEqualTo_def)




    have [simp]: "assign1 \<noteq> CallId us3"
      using False H65 a2 by auto
    have [simp]: "assign1 \<noteq> CallId us2"
      using H57 H65 \<open>assign1 \<noteq> CallId us3\<close> a2 by auto
    have [simp]: "assign1 \<noteq> CallId us1"
      using H49 H57 H65 \<open>assign1 \<noteq> CallId us2\<close> \<open>assign1 \<noteq> CallId us3\<close> a2 by auto
    have [simp]: "assign1 \<noteq> CallId us"
      using H41 H49 H57 H65 \<open>assign1 \<noteq> CallId us1\<close> \<open>assign1 \<noteq> CallId us2\<close> \<open>assign1 \<noteq> CallId us3\<close> a2 by auto


    text "These database calls cannot be from the new invocation, so they must have existed previously:"
    from a1 a2 a3
    have "task_columnid_isEqualTo t c1 \<lparr>contents = state_locallyGenerated_taskId\<rparr> \<lparr>contents = state_exposed_taskId\<rparr> \<lparr>contents = state_knownIds_taskId\<rparr>
            \<lparr>contents = state_locallyGenerated_columnId\<rparr> \<lparr>contents = state_exposed_columnId\<rparr> \<lparr>contents = state_knownIds_columnId\<rparr> \<lparr>contents = state_locallyGenerated_boardId\<rparr>
            \<lparr>contents = state_exposed_boardId\<rparr> \<lparr>contents = state_knownIds_boardId\<rparr> \<lparr>contents = state_callOps1\<rparr> \<lparr>contents = state_visibleCalls1\<rparr>
            \<lparr>contents = state_happensBefore1\<rparr> \<lparr>contents = state_callTransaction1\<rparr> \<lparr>contents = state_currentTransaction1\<rparr>
            \<lparr>contents = TransactionId state_currentTransactionId1\<rparr> \<lparr>contents = state_origin1\<rparr> \<lparr>contents = state_invocations2\<rparr> \<lparr>contents = state_invocationResult1\<rparr>
            \<lparr>contents = state_invocationHappensBefore1\<rparr>"
      apply (auto simp add: task_columnid_isEqualTo_def)
      apply (rule exI[where x=assign1])
      apply (auto simp add: task_columnid_isEqualTo_def H65 H66 H67 H57 H58 H59  H49 H50 H51 H41 H42 H43)
      apply (drule_tac x=c2 in spec)
      using us_distinct apply (auto simp add: H40 )
      using H41 H48 apply auto[1]
      using H41 H49 H56 apply auto[1]
      using H43 H51 H54 H57 H64 wellFormed_def apply auto[1]
      by (smt Select_neq fun_upd_same)


    with inv_pre
    have "column_tasks_contains c1 t \<lparr>contents = state_locallyGenerated_taskId\<rparr> \<lparr>contents = state_exposed_taskId\<rparr> \<lparr>contents = state_knownIds_taskId\<rparr>
            \<lparr>contents = state_locallyGenerated_columnId\<rparr> \<lparr>contents = state_exposed_columnId\<rparr> \<lparr>contents = state_knownIds_columnId\<rparr> \<lparr>contents = state_locallyGenerated_boardId\<rparr>
            \<lparr>contents = state_exposed_boardId\<rparr> \<lparr>contents = state_knownIds_boardId\<rparr> \<lparr>contents = state_callOps1\<rparr> \<lparr>contents = state_visibleCalls1\<rparr>
            \<lparr>contents = state_happensBefore1\<rparr> \<lparr>contents = state_callTransaction1\<rparr> \<lparr>contents = state_currentTransaction1\<rparr>
            \<lparr>contents = TransactionId state_currentTransactionId1\<rparr> \<lparr>contents = state_origin1\<rparr> \<lparr>contents = state_invocations2\<rparr> \<lparr>contents = state_invocationResult1\<rparr>
            \<lparr>contents = state_invocationHappensBefore1\<rparr>"
      by blast

    from this
    obtain add 
      where add1: "state_visibleCalls1 add"
        and add2: "state_callOps1 add = Op_column_tasks_add c1 t"
        and add3: "\<forall>rem. state_callOps1 rem = Op_column_tasks_remove c1 t \<longrightarrow> state_visibleCalls1 rem \<longrightarrow> \<not> state_happensBefore1 rem add"
      by (auto simp add: column_tasks_contains_def)


    show ?goal
    proof (rule exI[where x=add], auto)
      show "state_visibleCalls5 add"
        by (simp add: H43 H51 H59 H67 add1)
      show "state_callOps5 add = Op_column_tasks_add c1 t"
        by (metis H40 H41 H48 H49 H56 H57 H64 H65 add2 fun_upd_other operation.distinct(13))
      show "\<And>rem. \<lbrakk>state_callOps5 rem = Op_column_tasks_remove c1 t; state_visibleCalls5 rem; state_happensBefore5 rem add\<rbrakk> \<Longrightarrow> False"
        by (smt H41 H42 H43 H49 H50 H51 H57 H58 H59 H65 H66 H67 \<open>assign1 \<noteq> CallId us3\<close> a1 a3 add3 fun_upd_other fun_upd_same operation.distinct(295) operation.distinct(333) operation.distinct(357) operation.inject(8))


    qed
  qed
qed
why3_end

end
