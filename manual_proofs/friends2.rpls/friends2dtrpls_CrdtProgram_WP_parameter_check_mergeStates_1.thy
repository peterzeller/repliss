theory friends2dtrpls_CrdtProgram_WP_parameter_check_mergeStates_1
imports Why3
begin

why3_open "friends2dtrpls_CrdtProgram_WP_parameter_check_mergeStates_1.xml"

why3_vc WP_parameter_check_mergeStates


apply (auto simp add: friendSet_contains_def contents_def)
proof -

  from H29 obtain c1 where
        a1: "state_visibleCalls c1"
    and a2: "state_callOps c1 = Op_friendSet_add a b"
    and a3: "\<And>c2. \<lbrakk>state_visibleCalls c2; state_callOps c2 = Op_friendSet_remove a b\<rbrakk> \<Longrightarrow> state_happensBefore (c2, c1)"
    by (auto simp add: friendSet_contains_def contents_def)
    
  from a1 a2
  obtain c2 where   
          "state_visibleCalls c2"
      and "state_callOps c2 = Op_friendSet_add b a"
      and "state_sameTransaction (c1,c2)"
    by (smt H14 H15 H16 H19 H3 H5 H9 contents_def operation.distinct(1) ref.case wellFormed_def)
  
  have transactionVis1: "\<And>c1 c2. \<lbrakk>state_visibleCalls_1 c2; state_sameTransaction_1 (c2, c1)\<rbrakk> \<Longrightarrow> state_visibleCalls_1 c1"
      by (smt H1 contents_def ref.case wellFormed_def)
    
  have transactionVis2: "\<And>c1 c2. \<lbrakk>state_visibleCalls_2 c2; state_sameTransaction_2 (c2, c1)\<rbrakk> \<Longrightarrow> state_visibleCalls_2 c1"
      by (smt assms(2) contents_def ref.case wellFormed_def)
      
  have visCalls1: "state_callOps_1 c \<noteq> Noop" if "state_visibleCalls_1 c" for c    
    using that by (smt H1 contents_def ref.case wellFormed_def) 
      
  have visCalls2: "state_callOps_2 c \<noteq> Noop" if "state_visibleCalls_2 c" for c
      by (smt assms(2) contents_def ref.case that wellFormed_def)
  
  from H3 have transaction_hb: "\<And>x1 x2 y1 y2. \<lbrakk>state_happensBefore (x1, y1); state_sameTransaction (x1, x2); state_sameTransaction (y1, y2); \<not> state_sameTransaction (x1, y1)\<rbrakk> \<Longrightarrow> state_happensBefore (x2, y2)"
    by (smt contents_def ref.case  wellFormed_def)
  
      
  
  have "state_happensBefore (c3, c2)" if l1: "state_visibleCalls c3" and l2: "state_callOps c3 = Op_friendSet_remove b a"  for c3
  proof -
    from l1 l2
    have cases1: "state_callOps_1 c3 = Op_friendSet_remove b a 
        \<or> state_callOps_2 c3 = Op_friendSet_remove b a"
        by (metis H14 H15 H16)
    
    have cases2: "state_visibleCalls_1 c3 
        \<or> state_visibleCalls_2 c3"
        using H17 l1 by blast
         
    from cases1 cases2     
    have cases3: "state_visibleCalls_1 c3 \<and> state_callOps_1 c3 = Op_friendSet_remove b a 
        \<or> state_visibleCalls_2 c3 \<and> state_callOps_2 c3 = Op_friendSet_remove b a"
      using H14 H15 l2 visCalls1 visCalls2 by auto
              
          
    from this
    obtain c4 where 
          "state_visibleCalls c4"
      and "state_callOps c4 = Op_friendSet_remove a b"
      and "state_sameTransaction (c3, c4)"
      apply atomize_elim
      by (metis H10 H14 H15 H17 H19 H6 operation.distinct(4) transactionVis1 transactionVis2)
    
    have "state_happensBefore (c4, c1)"
       by (simp add: \<open>state_callOps c4 = Op_friendSet_remove a b\<close> \<open>state_visibleCalls c4\<close> a3)
    
    have cases_add: "state_callOps_1 c1 = Op_friendSet_add a b \<or> state_callOps_2 c1 = Op_friendSet_add a b"
      by (metis H14 H15 H16 a2)
      
    hence cases_add2: "state_callOps_1 c1 = Op_friendSet_add a b \<and> state_callOps_2 c1 = Op_friendSet_add a b
                     \<or> state_callOps_1 c1 = Noop \<and> state_callOps_2 c1 = Op_friendSet_add a b
                     \<or> state_callOps_1 c1 = Op_friendSet_add a b \<and> state_callOps_2 c1 = Noop"
      using H14 H15 a2 by auto
      
      
    have cases_remove: "state_callOps_1 c4 = Op_friendSet_remove a b \<or> state_callOps_2 c4 = Op_friendSet_remove a b"
      by (metis H14 H15 H16 \<open>state_callOps c4 = Op_friendSet_remove a b\<close>)  
    
    hence cases_remove2: "state_callOps_1 c4 = Op_friendSet_remove a b \<and> state_callOps_2 c4 = Op_friendSet_remove a b
                      \<or> state_callOps_1 c4 = Noop \<and> state_callOps_2 c4 = Op_friendSet_remove a b
                      \<or> state_callOps_1 c4 = Op_friendSet_remove a b \<and> state_callOps_2 c4 = Noop"
        using H14 H15 a2 by auto
      
    have cases_tr: "state_sameTransaction (x,y) = (state_sameTransaction_1 (x,y) \<or> state_sameTransaction_2 (x,y))" for x y
      using H19 by auto
    
    have callOps_cases: "state_callOps_1 c = state_callOps_2 c \<or> state_callOps_1 c = Noop \<or> state_callOps_2 c = Noop" for c
      using H14 H15 H16 by auto
    
    from H7 
    have sameTrans_add_rem1: "~state_sameTransaction_1 (x, y)" if "state_callOps_1 x = Op_friendSet_add a b" and "state_callOps_1 y = Op_friendSet_remove a b" for  a b x y
      by (simp add: that)
    
    from H11 
    have sameTrans_add_rem2: "~state_sameTransaction_2 (x, y)" if "state_callOps_2 x = Op_friendSet_add a b" and "state_callOps_2 y = Op_friendSet_remove a b" for  a b x y
      by (simp add: that)
      
    have transaction_sym1: "state_sameTransaction_1 (x, y) = state_sameTransaction_1 (y, x)" for x y
      using H1 by (smt contents_def ref.case  wellFormed_def)
    
    have transaction_sym2: "state_sameTransaction_2 (x, y) = state_sameTransaction_2 (y, x)" for x y
      using H2 by (smt contents_def ref.case  wellFormed_def)  
    
    have noop_notransL1: "\<not>state_sameTransaction_1 (x,y)" if "state_callOps_1 x = Noop" for x y
      using H1 that by (smt contents_def ref.case  wellFormed_def)
      
    have noop_notransR1: "\<not>state_sameTransaction_1 (x,y)" if "state_callOps_1 y = Noop" for x y
      using H1 that by (smt contents_def ref.case  wellFormed_def)
      
    have noop_notransL2: "\<not>state_sameTransaction_2 (x,y)" if "state_callOps_2 x = Noop" for x y
      using H2 that by (smt contents_def ref.case  wellFormed_def)
      
    have noop_notransR2: "\<not>state_sameTransaction_2 (x,y)" if "state_callOps_2 y = Noop" for x y
      using H2 that by (smt contents_def ref.case  wellFormed_def)
    
    have "\<not>state_sameTransaction_1 (c1,c4)"
      using cases_add2 cases_remove2 apply auto
      by (auto simp add: noop_notransL1 noop_notransR1 sameTrans_add_rem1)
    
    have "\<not>state_sameTransaction_2 (c1,c4)"
      using cases_add2 cases_remove2 apply auto
      by (auto simp add: noop_notransL2 noop_notransR2 sameTrans_add_rem2)  
      
    have "\<not>state_sameTransaction (c1,c4)"
      by (simp add: \<open>\<not> state_sameTransaction_1 (c1, c4)\<close> \<open>\<not> state_sameTransaction_2 (c1, c4)\<close> cases_tr)
    hence "\<not>state_sameTransaction (c4,c1)"
      by (simp add: cases_tr transaction_sym1 transaction_sym2)  
      
    
    from \<open>state_sameTransaction (c3, c4)\<close>
    have "state_sameTransaction (c4, c3)"
      by (smt H3 contents_def ref.case  wellFormed_def)
    
    from \<open>state_happensBefore (c4, c1)\<close> \<open>state_sameTransaction (c4, c3)\<close> \<open>state_sameTransaction (c1, c2)\<close> \<open>\<not>state_sameTransaction (c4,c1)\<close>
    show "state_happensBefore (c3, c2)"
      by (rule transaction_hb)
  qed    
  
  from this  
  show "\<exists>c2. state_visibleCalls c2 \<and> state_callOps c2 = Op_friendSet_add b a 
         \<and> (\<forall>c3. state_visibleCalls c3 \<and> state_callOps c3 = Op_friendSet_remove b a \<longrightarrow> state_happensBefore (c3, c2))"
    using \<open>state_callOps c2 = Op_friendSet_add b a\<close> \<open>state_visibleCalls c2\<close> by blast
qed         

why3_end

end
