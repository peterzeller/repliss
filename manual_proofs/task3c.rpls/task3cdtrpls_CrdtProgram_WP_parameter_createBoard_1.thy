theory task3cdtrpls_CrdtProgram_WP_parameter_createBoard_1
  imports Why3
begin

why3_open "task3cdtrpls_CrdtProgram_WP_parameter_createBoard_1.xml"

why3_vc WP_parameter_createBoard
proof -
  from H61
  obtain assign1 
    where a1: "state_visibleCalls2 assign1"
      and a2: "state_callOps2 assign1 = Op_task_columnid_assign t c1"
      and a3: "\<And>assign2 value. \<lbrakk>state_callOps2 assign2 = Op_task_columnid_assign t value;  state_visibleCalls2 assign2;  assign1 \<noteq> assign2\<rbrakk> \<Longrightarrow>  \<not> state_happensBefore2 assign2 assign1"
    by (auto simp add:  task_columnid_isEqualTo_def)

  thm H60[rule_format]

  have "\<exists>add. state_callOps2 add = Op_column_tasks_add c1 t \<and>
        state_callTransaction1 add = state_callTransaction1 assign1 \<and> (\<nexists>rem. state_callOps2 rem = Op_column_tasks_remove c1 t \<and> state_happensBefore2 rem add = True)"
  proof (rule H60[rule_format], auto simp add: a2)
    show "\<And>assign2 c2. \<lbrakk>assign1 \<noteq> assign2; state_happensBefore2 assign2 assign1; state_callOps2 assign2 = Op_task_columnid_assign t c2\<rbrakk> \<Longrightarrow> False"
      apply (drule a3, auto)
      thm a3[rule_format]


      show ?thesis
        apply (auto simp add: column_tasks_contains_def)

why3_end

end
