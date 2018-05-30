theory userbasedtrpls_CrdtProgram_WP_parameter_registerUser_2
imports Why3
begin

why3_open "userbasedtrpls_CrdtProgram_WP_parameter_registerUser_2.xml"

why3_vc WP_parameter_registerUser
proof (simp, intro allI impI disjCI, simp)
  fix "write" delete u f v
  assume a0_3: "state_callOps3 delete = Op_mapDelete u"
     and a1_3: "state_happensBefore3 (delete, write)"

  from a0_3
  have a0_2:"state_callOps2 delete = Op_mapDelete u"
    by (metis H60 H61 operation.distinct(10))
  hence a0_1: "state_callOps1 delete = Op_mapDelete u"
    by (metis H50 H51 operation.distinct(10))
    

    
     
  find_theorems state_callOps
  
  show "state_callOps3 write \<noteq> Op_mapWrite u f v"
  proof (cases "write = CallId us1")
    case True
      then show ?thesis sorry
  next
    case False
      hence write_not_us1: "write \<noteq> CallId us1" .
      
      then show ?thesis 
      proof (cases "write = CallId us")
        case True
          find_theorems us
          then show ?thesis  sorry
      next
        case False
          hence write_not_us: "write \<noteq> CallId us" .
        
          from a1_3
          have a1_2: "state_happensBefore2 (delete, write)"
            using H62 write_not_us1 by blast 
          hence a1_1: "state_happensBefore1 (delete, write)"
            using H52 write_not_us by blast
        
          have "state_callOps1 write \<noteq> Op_mapWrite u f v"
            using H37 a0_1 a1_1 by blast  
          
          then show ?thesis
            by (simp add: H51 H61 write_not_us write_not_us1) 
        
      qed
      
  qed

  find_theorems state_callOps2

why3_end

end
