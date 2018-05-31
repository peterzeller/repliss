theory task3edtrpls_CrdtProgram_WP_parameter_moveTask_1
imports Why3
begin

why3_open "task3edtrpls_CrdtProgram_WP_parameter_moveTask_1.xml"

why3_vc WP_parameter_moveTask
proof -
  have "state_callOps2 c = state_callOps1 c" if "state_callOps1 c \<noteq> Noop" for c
    by (metis H40 H41 fun_upd_def that)
  hence "state_callOps3 c = state_callOps1 c" if "state_callOps1 c \<noteq> Noop" for c
    using H48 H49 that by auto
  hence "state_callOps4 c = state_callOps1 c" if "state_callOps1 c \<noteq> Noop" for c
    by (metis H56 H57 fun_upd_apply that)
  hence state_callOps1_mono:  "state_callOps5 c = state_callOps1 c" if "state_callOps1 c \<noteq> Noop" for c
    by (metis H64 H65 fun_upd_apply that)

  have happensBefore_mono: "state_happensBefore5 c1 c2" if "state_happensBefore1 c1 c2" for c1 c2
    by (smt H25 H40 H41 H42 H48 H49 H50 H56 H58 H64 H66 \<open>\<And>c. state_callOps1 c \<noteq> Noop \<Longrightarrow> state_callOps4 c = state_callOps1 c\<close> fun_upd_other select_convs(1) that wellFormed_def)




  have "us2 \<noteq> us3"
    using H57 H64 by auto

  have "us1 \<noteq> us2"
    using H49 H56 by auto
  have "us1 \<noteq> us3"
    using H49 H57 H64 \<open>us2 \<noteq> us3\<close> by auto

  have "us \<noteq> us1"
    using H41 H48 by auto
  have "us \<noteq> us2"
    using H41 H49 H56 \<open>us \<noteq> us1\<close> by auto
  have "us \<noteq> us3"
    using H41 H49 H57 H64 \<open>us1 \<noteq> us3\<close> \<open>us2 \<noteq> us3\<close> by auto




  find_theorems CallId Op_column_tasks_add

  show " \<exists>add. state_callOps5 add = Op_column_tasks_add c1 t \<and>
          state_callTransaction1 add = state_callTransaction1 assign1 \<and>
          (\<forall>rem. state_callOps5 rem = Op_column_tasks_remove c1 t \<and> state_happensBefore5 rem add = True \<longrightarrow>
                 (\<exists>assign2 c2.
                     assign1 \<noteq> assign2 \<and>
                     state_callOps5 assign2 = Op_task_columnid_assign t c2 \<and>
                     state_callTransaction1 rem = state_callTransaction1 assign2 \<and> state_happensBefore5 assign2 assign1 = True))"
  proof (cases "assign1 = CallId us3")
    case True
    define add where "add \<equiv> (CallId us2)"

    show ?thesis
    proof
      show "state_callOps5 add = Op_column_tasks_add c1 t \<and>
      state_callTransaction1 add = state_callTransaction1 assign1 \<and>
      (\<forall>rem. state_callOps5 rem = Op_column_tasks_remove c1 t \<and> state_happensBefore5 rem add = True \<longrightarrow>
             (\<exists>assign2 c2.
                 assign1 \<noteq> assign2 \<and>
                 state_callOps5 assign2 = Op_task_columnid_assign t c2 \<and> state_callTransaction1 rem = state_callTransaction1 assign2 \<and> state_happensBefore5 assign2 assign1 = True))"
        apply auto
        using H57 H65 H72 True \<open>us2 \<noteq> us3\<close> add_def apply auto[1]
         apply (simp add: H61 H69 True add_def)
        find_theorems wellFormed (* TODO maybe inline important wellFormed conditions *)
        thm H46 H49 H50 H51 H56 H57 H58 H65 H66 add_def fun_upd_apply operation.distinct(15) operation.distinct(295) operation.distinct(333) select_convs(1) wellFormed_def
        by (smt H46 H49 H50 H51 H56 H57 H58 H65 H66 add_def fun_upd_apply operation.distinct(15) operation.distinct(295) operation.distinct(333) select_convs(1) wellFormed_def)
    qed
  next
    case False
    hence False: "assign1 \<noteq> CallId us3" .

  (* use old invariant: *)
    thm H26[rule_format, of assign1]

    have assign1_op: "state_callOps1 assign1 = Op_task_columnid_assign t c1"
      thm False H41 H49 H57 H65 H72 Select_eq fun_upd_other operation.distinct(301) operation.distinct(333) operation.distinct(441)
      by (smt False H41 H49 H57 H65 H72 Select_eq fun_upd_other operation.distinct(301) operation.distinct(333) operation.distinct(441))

    from H26[rule_format, OF assign1_op]
    obtain add 
      where add1: "state_callOps1 add = Op_column_tasks_add c1 t"
        and add2: "state_callTransaction1 add = state_callTransaction1 assign1"
        and add3: "(\<forall>rem. state_callOps1 rem = Op_column_tasks_remove c1 t \<and> state_happensBefore1 rem add = True \<longrightarrow>
             (\<exists>assign2 c2.
                 assign1 \<noteq> assign2 \<and>
                 state_callOps1 assign2 = Op_task_columnid_assign t c2 \<and> state_callTransaction1 rem = state_callTransaction1 assign2 \<and> state_happensBefore1 assign2 assign1 = True))"
      by auto


    show ?thesis 
    proof
      show "state_callOps5 add = Op_column_tasks_add c1 t \<and>
        state_callTransaction1 add = state_callTransaction1 assign1 \<and>
        (\<forall>rem. state_callOps5 rem = Op_column_tasks_remove c1 t \<and> state_happensBefore5 rem add = True \<longrightarrow>
           (\<exists>assign2 c2.
               assign1 \<noteq> assign2 \<and>
               state_callOps5 assign2 = Op_task_columnid_assign t c2 \<and> state_callTransaction1 rem = state_callTransaction1 assign2 \<and> state_happensBefore5 assign2 assign1 = True))"
      proof auto
        show "state_callOps5 add = Op_column_tasks_add c1 t"
          by (simp add: add1 state_callOps1_mono)
        show "state_callTransaction1 add = state_callTransaction1 assign1"
          by (simp add: add2)

        find_theorems task_columnid_get


        define task_get_res where "task_get_res \<equiv> task_columnid_get task_id \<lparr>contents = state_locallyGenerated_taskId\<rparr> \<lparr>contents = state_exposed_taskId\<rparr> \<lparr>contents = state_knownIds_taskId\<rparr>
          \<lparr>contents = state_locallyGenerated_columnId\<rparr> \<lparr>contents = state_exposed_columnId\<rparr> \<lparr>contents = state_knownIds_columnId\<rparr> \<lparr>contents = state_locallyGenerated_boardId\<rparr>
          \<lparr>contents = state_exposed_boardId\<rparr> \<lparr>contents = state_knownIds_boardId\<rparr> \<lparr>contents = state_callOps1\<rparr> \<lparr>contents = state_visibleCalls1\<rparr> \<lparr>contents = state_happensBefore1\<rparr>
          \<lparr>contents = state_callTransaction1\<rparr> \<lparr>contents = state_currentTransaction\<rparr> \<lparr>contents = TransactionId state_currentTransactionId\<rparr> \<lparr>contents = state_origin1\<rparr>
          \<lparr>contents = state_invocations2\<rparr> \<lparr>contents = state_invocationResult1\<rparr> \<lparr>contents = state_invocationHappensBefore1\<rparr>"

        have H49': "state_callOps3 = state_callOps2
             (CallId us1 :=
                Op_column_tasks_remove
                 task_get_res
                 task_id)"
          by (simp add: H49 task_get_res_def)


        find_theorems CallId Op_column_tasks_remove
        have rem_cases:"state_callOps5 rem = Op_column_tasks_remove c1 t 
            \<longleftrightarrow> (state_callOps1 rem = Op_column_tasks_remove c1 t \<or> (rem = CallId us1 \<and> t = task_id \<and> c1 = task_get_res))" for rem
          apply (auto simp add: H65 H57 H49' H41 H40 \<open>us \<noteq> us2\<close> \<open>us \<noteq> us3\<close> \<open>us \<noteq> us1\<close> \<open>us1 \<noteq> us2\<close> \<open>us1 \<noteq> us3\<close> \<open>us2 \<noteq> us3\<close> split: if_splits)
          using H48 \<open>\<And>c. state_callOps1 c \<noteq> Noop \<Longrightarrow> state_callOps2 c = state_callOps1 c\<close> apply auto[1]
          using H48 \<open>\<And>c. state_callOps1 c \<noteq> Noop \<Longrightarrow> state_callOps2 c = state_callOps1 c\<close> apply auto[1]
          using H56 \<open>\<And>c. state_callOps1 c \<noteq> Noop \<Longrightarrow> state_callOps3 c = state_callOps1 c\<close> apply auto[1]
          using H64 \<open>\<And>c. state_callOps1 c \<noteq> Noop \<Longrightarrow> state_callOps4 c = state_callOps1 c\<close> by auto




        show "\<And>rem. \<lbrakk>state_callOps5 rem = Op_column_tasks_remove c1 t; state_happensBefore5 rem add\<rbrakk>
           \<Longrightarrow> \<exists>assign2. assign1 \<noteq> assign2 \<and>
                          (\<exists>c2. state_callOps5 assign2 = Op_task_columnid_assign t c2) \<and>
                          state_callTransaction1 rem = state_callTransaction1 assign2 \<and> state_happensBefore5 assign2 assign1"
        proof (auto simp add: rem_cases)
          show "\<And>rem. \<lbrakk>state_happensBefore5 rem add; state_callOps1 rem = Op_column_tasks_remove c1 t\<rbrakk>
           \<Longrightarrow> \<exists>assign2. assign1 \<noteq> assign2 \<and>
                          (\<exists>c2. state_callOps5 assign2 = Op_task_columnid_assign t c2) \<and>
                          state_callTransaction1 rem = state_callTransaction1 assign2 \<and> state_happensBefore5 assign2 assign1"
            by (smt H40 H41 H42 H48 H50 H57 H58 H65 H66 Select_eq add3 fun_upd_triv fun_upd_twist happensBefore_mono operation.distinct(15) operation.distinct(21) operation.distinct(295) operation.distinct(333) state_callOps1_mono)


          define assign2 where "assign2 \<equiv> CallId us3"
          find_theorems Op_task_columnid_assign CallId
          show "\<exists>assign2. assign1 \<noteq> assign2 \<and>
                   (\<exists>c2. state_callOps5 assign2 = Op_task_columnid_assign task_id c2) \<and>
                   state_callTransaction1 (CallId us1) = state_callTransaction1 assign2 \<and> state_happensBefore5 assign2 assign1"
            if "state_happensBefore5 (CallId us1) add" "c1 = task_get_res" "t = task_id"
          proof (rule exI[where x="CallId us3"])
            show "assign1 \<noteq> CallId us3 \<and>
              (\<exists>c2. state_callOps5 (CallId us3) = Op_task_columnid_assign task_id c2) \<and>
              state_callTransaction1 (CallId us1) = state_callTransaction1 (CallId us3) \<and> state_happensBefore5 (CallId us3) assign1"
            proof (intro conjI)
              show " assign1 \<noteq> CallId us3"
                by (simp add: False)
              show "\<exists>c2. state_callOps5 (CallId us3) = Op_task_columnid_assign task_id c2"
                by (simp add: H65)
              show "state_callTransaction1 (CallId us1) = state_callTransaction1 (CallId us3)"
                by (simp add: H53 H69)
              have us3_before_1: "state_happensBefore5 (CallId us3) (CallId us1)"
                by (simp add: H51 H59 H66 H67)


              have "add \<noteq> CallId us1"
                using H48 \<open>\<And>c. state_callOps1 c \<noteq> Noop \<Longrightarrow> state_callOps2 c = state_callOps1 c\<close> add1 by auto

              have "add \<noteq> CallId us"
                using H40 add1 by auto


(*
              thm add2
              from assign1_op
              have " state_callOps assign1 = Op_task_columnid_assign t c1"
*)
                find_theorems assign1

              from `state_happensBefore5 (CallId us1) add`
              have "state_visibleCalls1 add"
                by (auto simp add: H66  `us1 \<noteq> us3` H58 `us1 \<noteq> us2` H51 H50 `add \<noteq> CallId us1` H42 H43 `add \<noteq> CallId us`)

              hence "state_visibleCalls1 assign1"
                using H28 add2 assign1_op by auto 

                find_theorems state_visibleCalls1 state_callTransaction1
                find_theorems  state_visibleCalls1 


              show "state_happensBefore5 (CallId us3) assign1"
                using `state_happensBefore5 (CallId us1) add` add2 us3_before_1
                by (simp add: H43 H51 H59 H66 H67 \<open>state_visibleCalls1 assign1\<close>)
            qed
          qed
        qed
      qed
    qed
  qed
qed


why3_end

end
