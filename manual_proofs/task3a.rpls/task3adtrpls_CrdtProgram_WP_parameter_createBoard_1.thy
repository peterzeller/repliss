theory task3adtrpls_CrdtProgram_WP_parameter_createBoard_1
imports Why3
begin

why3_open "task3adtrpls_CrdtProgram_WP_parameter_createBoard_1.xml"

why3_vc WP_parameter_createBoard
proof -
  from H62
  obtain c1a 
    where a1 : "state_visibleCalls2 c1a"
and a2: "state_callOps2 c1a = Op_task_columnid_assign t c1"
and a_latest: "\<forall>d. state_callOps2 d = Op_task_delete t \<longrightarrow> \<not> state_happensBefore2 d c1a"
            \<forall>c2. state_callOps2 c2 = Op_task_columnid_assign t c1 \<longrightarrow>
                 state_visibleCalls2 c2 \<longrightarrow> c1a = c2 \<or> (\<exists>d. state_callOps2 d = Op_task_delete t \<and> state_happensBefore2 d c2) \<or> \<not> state_happensBefore2 c2 c1a\<rbrakk>
    apply (auto simp add: task_columnid_isEqualTo_def)

  show "column_tasks_contains c1 t \<lparr>contents = state_locallyGenerated_taskId\<rparr> \<lparr>contents = state_exposed_taskId\<rparr> \<lparr>contents = state_knownIds_taskId\<rparr>
     \<lparr>contents = state_locallyGenerated_columnId\<rparr> \<lparr>contents = state_exposed_columnId\<rparr> \<lparr>contents = state_knownIds_columnId\<rparr> \<lparr>contents = state_locallyGenerated_boardId(o1 := True)\<rparr>
     \<lparr>contents = state_exposed_boardId\<rparr> \<lparr>contents = state_knownIds_boardId\<rparr> \<lparr>contents = state_callOps2\<rparr> \<lparr>contents = state_visibleCalls2\<rparr> \<lparr>contents = state_happensBefore2\<rparr>
     \<lparr>contents = state_callTransaction1\<rparr> \<lparr>contents = state_currentTransaction2\<rparr> \<lparr>contents = TransactionId state_currentTransactionId1\<rparr> \<lparr>contents = state_origin2\<rparr>
     \<lparr>contents = state_invocations2\<rparr> \<lparr>contents = state_invocationResult1\<rparr> \<lparr>contents = state_invocationHappensBefore1\<rparr>"
  proof (auto simp add: column_tasks_contains_def)
    show "\<exists>c1a. state_visibleCalls2 c1a \<and>
          state_callOps2 c1a = Op_column_tasks_add c1 t \<and>
          (\<forall>d. state_callOps2 d = Op_column_delete c1 \<longrightarrow> \<not> state_happensBefore2 d c1a) \<and>
          (\<forall>c2. state_callOps2 c2 = Op_column_tasks_remove c1 t \<longrightarrow>
                state_visibleCalls2 c2 \<longrightarrow> (\<exists>d. state_callOps2 d = Op_column_delete c1 \<and> state_happensBefore2 d c2) \<or> \<not> state_happensBefore2 c2 c1a)"

why3_end

end
