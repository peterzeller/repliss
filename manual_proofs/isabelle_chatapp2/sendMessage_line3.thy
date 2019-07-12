
theory sendMessage_line3
  imports Main
begin

      datatype CallId = CallId nat

datatype TxId = TxId nat

datatype MessageId = MessageId nat

datatype String = String nat

datatype UserId = UserId nat

datatype callInfo =
    queryop_message_content_mv_contains (key8: "MessageId") (elem4: "String") (result6: "bool")
  | queryop_messages_contains (elem2: "MessageId") (result: "bool")
  | message_content_assign (key1: "MessageId") (value1: "String")
  | message_delete (key2: "MessageId")
  | queryop_message_content_get (key6: "MessageId") (result4: "String")
  | queryop_message_author_getFirst (key4: "MessageId") (result2: "UserId")
  | messages_remove (elem1: "MessageId")
  | no_call 
  | message_author_assign (key: "MessageId") (qvalue: "UserId")
  | queryop_message_exists (key9: "MessageId") (result7: "bool")
  | queryop_message_content_getFirst (key7: "MessageId") (result5: "String")
  | queryop_message_author_get (key3: "MessageId") (result1: "UserId")
  | messages_add (elem: "MessageId")
  | queryop_message_author_mv_contains (key5: "MessageId") (elem3: "UserId") (result3: "bool")

datatype InvocationId = InvocationId nat

datatype invocationResult =
    sendMessage_res (sendMessage_res_arg: "MessageId")
  | NoResult 

datatype invocationInfo =
    sendMessage (qfrom: "UserId") (content: "String")
  | no_invocation 

lemma "sendMessage_line3":
fixes happensBefore3 :: "CallId => CallId set"
         
fixes calls4 :: "CallId => callInfo"
         
fixes knownIds_MessageId1 :: "MessageId set"
         
fixes message_author_mv_contains_res6 :: "MessageId => UserId => bool"
         
fixes transactionOrigin1 :: "TxId => InvocationId option"
         
fixes messages_contains_res2 :: "MessageId => bool"
         
fixes c11 :: "CallId"
         
fixes invocationRes :: "InvocationId => invocationResult"
         
fixes message_exists_res4 :: "MessageId => bool"
         
fixes message_author_mv_contains_res :: "MessageId => UserId => bool"
         
fixes message_author_mv_contains_res1 :: "MessageId => UserId => bool"
         
fixes vis1 :: "CallId set"
         
fixes messages_contains_res4 :: "MessageId => bool"
         
fixes calls1 :: "CallId => callInfo"
         
fixes content_init :: "String"
         
fixes message_author_mv_contains_res5 :: "MessageId => UserId => bool"
         
fixes messages_contains_res :: "MessageId => bool"
         
fixes invocationOp :: "InvocationId => invocationInfo"
         
fixes callOrigin :: "CallId => TxId option"
         
fixes messages_contains_res3 :: "MessageId => bool"
         
fixes happensBefore4 :: "CallId => CallId set"
         
fixes invocationCalls :: "InvocationId => CallId set"
         
fixes calls :: "CallId => callInfo"
         
fixes calls3 :: "CallId => callInfo"
         
fixes newCalls :: "CallId set"
         
fixes m :: "MessageId"
         
fixes vis :: "CallId set"
         
fixes message_exists_res6 :: "MessageId => bool"
         
fixes transactionOrigin :: "TxId => InvocationId option"
         
fixes callOrigin1 :: "CallId => TxId option"
         
fixes message_author_mv_contains_res2 :: "MessageId => UserId => bool"
         
fixes message_exists_res3 :: "MessageId => bool"
         
fixes vis2 :: "CallId set"
         
fixes calls2 :: "CallId => callInfo"
         
fixes c21 :: "CallId"
         
fixes snapshotAddition :: "CallId set"
         
fixes happensBefore1 :: "CallId => CallId set"
         
fixes generatedIds_MessageId1 :: "MessageId => InvocationId option"
         
fixes generatedIds_MessageId :: "MessageId => InvocationId option"
         
fixes message_exists_res7 :: "MessageId => bool"
         
fixes message_author_mv_contains_res4 :: "MessageId => UserId => bool"
         
fixes snapshotAddition1 :: "CallId set"
         
fixes currentInvocation :: "InvocationId"
         
fixes message_exists_res2 :: "MessageId => bool"
         
fixes message_exists_res8 :: "MessageId => bool"
         
fixes invocationCalls1 :: "InvocationId => CallId set"
         
fixes messages_contains_res1 :: "MessageId => bool"
         
fixes c0 :: "CallId"
         
fixes vis3 :: "CallId set"
         
fixes knownIds_MessageId :: "MessageId set"
         
fixes message_exists_res :: "MessageId => bool"
         
fixes happensBefore2 :: "CallId => CallId set"
         
fixes message_exists_res1 :: "MessageId => bool"
         
fixes from_init :: "UserId"
         
fixes message_exists_res5 :: "MessageId => bool"
         
fixes message_exists_res9 :: "MessageId => bool"
         
fixes tx :: "TxId"
         
fixes message_author_mv_contains_res7 :: "MessageId => UserId => bool"
         
fixes message_author_mv_contains_res3 :: "MessageId => UserId => bool"
         
fixes messages_contains_res5 :: "MessageId => bool"
         
fixes happensBefore :: "CallId => CallId set"
         
fixes newTxns :: "TxId set"
         
assumes before_procedure_invocation_snapshot_addition_transaction_consistent:

        "(∀bound_c116.
         (∀bound_c216.
           (((bound_c116 ∈ snapshotAddition) ∧ (bound_c216 ∈ (happensBefore bound_c116))) ⟶ (bound_c216 ∈ snapshotAddition))))"
         
assumes before_procedure_invocation_snapshot_addition_transaction_consistent_2:

        "(∀bound_c115.
         (∀bound_c215.
           (((bound_c115 ∈ snapshotAddition) ∧ ((callOrigin bound_c115) = (callOrigin bound_c215)))
             ⟶ (bound_c215 ∈ snapshotAddition))))"
         
assumes before_procedure_invocation_snapshot_addition_subset_calls:

        "(∀bound_c31. ((bound_c31 ∈ snapshotAddition) ⟶ ((calls bound_c31) ≠ no_call)))"
         
assumes before_procedure_invocation_MessageId_knownIds_are_generated:

        "(∀bound_x3. ((bound_x3 ∈ knownIds_MessageId) ⟶ ¬((generatedIds_MessageId bound_x3) = None)))"
         
assumes before_procedure_invocation_message_delete_call_parameter_key_generated:

        "(∀bound_c30.
         (∀bound_key2. (((calls bound_c30) = (message_delete bound_key2)) ⟶ ¬((generatedIds_MessageId bound_key2) = None))))"
         
assumes before_procedure_invocation_message_content_assign_call_parameter_key_generated:

        "(∀bound_c20.
         (∀bound_key1.
           (∀bound_value1.
             (((calls bound_c20) = (message_content_assign bound_key1 bound_value1))
               ⟶ ¬((generatedIds_MessageId bound_key1) = None)))))"
         
assumes before_procedure_invocation_message_author_assign_call_parameter_key_generated:

        "(∀bound_c10.
         (∀bound_key.
           (∀bound_value.
             (((calls bound_c10) = (message_author_assign bound_key bound_value))
               ⟶ ¬((generatedIds_MessageId bound_key) = None)))))"
         
assumes before_procedure_invocation_messages_remove_call_parameter_elem_generated:

        "(∀bound_c9.
         (∀bound_elem1. (((calls bound_c9) = (messages_remove bound_elem1)) ⟶ ¬((generatedIds_MessageId bound_elem1) = None))))"
         
assumes before_procedure_invocation_messages_add_call_parameter_elem_generated:

        "(∀bound_c8.
         (∀bound_elem. (((calls bound_c8) = (messages_add bound_elem)) ⟶ ¬((generatedIds_MessageId bound_elem) = None))))"
         
assumes before_procedure_invocation_sendMessage_result_known:

        "(∀bound_i4.
         (∀bound_result. (((invocationRes bound_i4) = (sendMessage_res bound_result)) ⟶ (bound_result ∈ knownIds_MessageId))))"
         
assumes before_procedure_invocation_WF_transactionOrigin_exists:

        "(∀bound_tx6.
         (∀bound_i3. (((transactionOrigin bound_tx6) = (Some bound_i3)) ⟶ ((invocationOp bound_i3) ≠ no_invocation))))"
         
assumes before_procedure_invocation_WF_callOrigin_exists:

        "(∀bound_ca1. (∀bound_tx5. (((callOrigin bound_ca1) = (Some bound_tx5)) ⟶ ¬((transactionOrigin bound_tx5) = None))))"
         
assumes before_procedure_invocation_WF_no_call_implies_not_in_happensBefore:

        "(∀bound_ca. (∀bound_cb. (((callOrigin bound_ca) = None) ⟶ ¬(bound_ca ∈ (happensBefore bound_cb)))))"
         
assumes before_procedure_invocation_WF_no_call_implies_no_happensBefore:

        "(∀bound_c7. (((callOrigin bound_c7) = None) ⟶ ((happensBefore bound_c7) = {})))"
         
assumes before_procedure_invocation_WF_transactionOrigin_callOrigin:

        "(∀bound_tx4. (((transactionOrigin bound_tx4) = None) ⟶ (∀bound_c6. ((callOrigin bound_c6) ≠ (Some bound_tx4)))))"
         
assumes before_procedure_invocation_WF_callOrigin:

        "(∀bound_c5. (((callOrigin bound_c5) = None) = ((calls bound_c5) = no_call)))"
         
assumes before_procedure_invocation_WF_invocationCalls:

        "(∀bound_i2.
         (∀bound_c4.
           ((bound_c4 ∈ (invocationCalls bound_i2))
             = (∃bound_tx3. (((callOrigin bound_c4) = (Some bound_tx3)) ∧ ((transactionOrigin bound_tx3) = (Some bound_i2)))))))"
         
assumes before_procedure_invocation_no_invocation_implies_no_result:

        "(∀bound_x11.
         (∀bound_x2.
           (∀bound_y11.
             (∀bound_y21.
               ((((((callOrigin bound_x11) = (callOrigin bound_x2)) ∧ ((callOrigin bound_y11) = (callOrigin bound_y21)))
                 ∧ ¬((callOrigin bound_x11) = (callOrigin bound_y11)))
                 ∧ (bound_x2 ∈ (happensBefore bound_y11)))
                 ⟶ (bound_x2 ∈ (happensBefore bound_y21)))))))"
         
assumes before_procedure_invocation_no_invocation_implies_no_result_2:

        "(∀bound_i1. (((invocationOp bound_i1) = no_invocation) ⟶ ((invocationRes bound_i1) = NoReturn)))"
         
assumes before_procedure_invocation_happensBefore_antisym:

        "(∀bound_x1.
         (∀bound_y1. (((bound_x1 ∈ (happensBefore bound_y1)) ∧ (bound_y1 ∈ (happensBefore bound_x1))) ⟶ (bound_x1 = bound_y1))))"
         
assumes before_procedure_invocation_happensBefore_trans:

        "(∀bound_x.
         (∀bound_y.
           (∀bound_z.
             (((bound_x ∈ (happensBefore bound_y)) ∧ (bound_y ∈ (happensBefore bound_z))) ⟶ (bound_x ∈ (happensBefore bound_y))))))"
         
assumes before_procedure_invocation_happensBefore_reflex:

        "(∀bound_c3. (((calls bound_c3) ≠ no_call) ⟶ (bound_c3 ∈ (happensBefore bound_c3))))"
         
assumes before_procedure_invocation_visibleCalls_causally_consistent:

        "(∀bound_c114. (∀bound_c214. (((bound_c214 ∈ {}) ∧ (bound_c114 ∈ (happensBefore bound_c214))) ⟶ (bound_c114 ∈ {}))))"
         
assumes before_procedure_invocation_visibleCalls_transaction_consistent1:

        "(∀bound_c113.
         (∀bound_c213.
           ((((bound_c113 ∈ {}) ∧ ((callOrigin bound_c113) = (callOrigin bound_c213))) ∧ ((calls bound_c213) ≠ no_call))
             ⟶ (bound_c213 ∈ {}))))"
         
assumes before_procedure_invocation_visibleCalls_exist:

        "(∀bound_c. ((bound_c ∈ {}) ⟶ ((calls bound_c) ≠ no_call)))"
         
assumes before_procedure_invocation_invocation_sequential:

        "(∀bound_c112.
         (∀bound_tx1.
           (∀bound_i.
             (∀bound_c212.
               (∀bound_tx2.
                 ((((((callOrigin bound_c112) = (Some bound_tx1)) ∧ ((transactionOrigin bound_tx1) = (Some bound_i)))
                   ∧ ((callOrigin bound_c212) = (Some bound_tx2)))
                   ∧ ((transactionOrigin bound_tx2) = (Some bound_i)))
                   ⟶ ((bound_c112 ∈ (happensBefore bound_c212)) ∨ (bound_c212 ∈ (happensBefore bound_c112)))))))))"
         
assumes before_procedure_invocation_happensBefore_exists_r:

        "(∀bound_c111. (∀bound_c211. (((calls bound_c111) = no_call) ⟶ ¬(bound_c111 ∈ (happensBefore bound_c211)))))"
         
assumes before_procedure_invocation_happensBefore_exists_l:

        "(∀bound_c110. (∀bound_c210. (((calls bound_c110) = no_call) ⟶ ¬(bound_c110 ∈ (happensBefore bound_c210)))))"
         
assumes no_call_in_new_invocation:

        "((invocationCalls currentInvocation) = {})"
         
assumes message_exists_res_def:

        "(∀bound_m2.
         ((message_exists_res bound_m2)
           = (∃bound_c14.
           ((((bound_c14 ∈ snapshotAddition)
             ∧ (∃bound_args4. ((calls bound_c14) = (message_author_assign bound_m2 bound_args4))))
             ∨ ((bound_c14 ∈ snapshotAddition)
               ∧ (∃bound_args5. ((calls bound_c14) = (message_content_assign bound_m2 bound_args5)))))
             ∧ (∀bound_c24.
               (((bound_c24 ∈ snapshotAddition) ∧ ((calls bound_c24) = (message_delete bound_m2)))
                 ⟶ (bound_c24 ∈ (happensBefore bound_c14))))))))"
         
assumes message_author_mv_contains_res_def:

        "(∀bound_m2.
         (∀bound_a1.
           (((message_author_mv_contains_res bound_m2) bound_a1)
             = (∃bound_c15.
             (((bound_c15 ∈ snapshotAddition)
               ∧ (((calls bound_c15) = (message_author_assign bound_m2 bound_a1))
                 ∧ (∀bound_d.
                   (((bound_d ∈ snapshotAddition) ∧ ((calls bound_d) = (message_delete bound_m2)))
                     ⟶ (bound_d ∈ (happensBefore bound_c15))))))
               ∧ ¬(∃bound_c25.
                 (∃bound_anyArgs.
                   ((((bound_c25 ∈ snapshotAddition) ∧ (bound_c15 ≠ bound_c25))
                     ∧ (((calls bound_c25) = (message_author_assign bound_m2 bound_anyArgs))
                       ∧ (∀bound_d1.
                         (((bound_d1 ∈ snapshotAddition) ∧ ((calls bound_d1) = (message_delete bound_m2)))
                           ⟶ (bound_d1 ∈ (happensBefore bound_c25))))))
                     ∧ (bound_c15 ∈ (happensBefore bound_c25))))))))))"
         
assumes message_author_mv_contains_res_def_2:

        "(∀bound_m2.
         (∀bound_a2.
           (((message_author_mv_contains_res1 bound_m2) bound_a2)
             = (∃bound_c16.
             (((bound_c16 ∈ snapshotAddition)
               ∧ (((calls bound_c16) = (message_author_assign bound_m2 bound_a2))
                 ∧ (∀bound_d2.
                   (((bound_d2 ∈ snapshotAddition) ∧ ((calls bound_d2) = (message_delete bound_m2)))
                     ⟶ (bound_d2 ∈ (happensBefore bound_c16))))))
               ∧ ¬(∃bound_c26.
                 (∃bound_anyArgs1.
                   ((((bound_c26 ∈ snapshotAddition) ∧ (bound_c16 ≠ bound_c26))
                     ∧ (((calls bound_c26) = (message_author_assign bound_m2 bound_anyArgs1))
                       ∧ (∀bound_d3.
                         (((bound_d3 ∈ snapshotAddition) ∧ ((calls bound_d3) = (message_delete bound_m2)))
                           ⟶ (bound_d3 ∈ (happensBefore bound_c26))))))
                     ∧ (bound_c16 ∈ (happensBefore bound_c26))))))))))"
         
assumes message_exists_res_def_2:

        "(∀bound_m3.
         ((message_exists_res1 bound_m3)
           = (∃bound_c17.
           ((((bound_c17 ∈ ({} ∪ snapshotAddition))
             ∧ (∃bound_args6. ((calls bound_c17) = (message_author_assign bound_m3 bound_args6))))
             ∨ ((bound_c17 ∈ ({} ∪ snapshotAddition))
               ∧ (∃bound_args7. ((calls bound_c17) = (message_content_assign bound_m3 bound_args7)))))
             ∧ (∀bound_c27.
               (((bound_c27 ∈ ({} ∪ snapshotAddition)) ∧ ((calls bound_c27) = (message_delete bound_m3)))
                 ⟶ (bound_c27 ∈ (happensBefore bound_c17))))))))"
         
assumes message_author_mv_contains_res_def_3:

        "(∀bound_m3.
         (∀bound_a11.
           (((message_author_mv_contains_res2 bound_m3) bound_a11)
             = (∃bound_c18.
             (((bound_c18 ∈ ({} ∪ snapshotAddition))
               ∧ (((calls bound_c18) = (message_author_assign bound_m3 bound_a11))
                 ∧ (∀bound_d4.
                   (((bound_d4 ∈ ({} ∪ snapshotAddition)) ∧ ((calls bound_d4) = (message_delete bound_m3)))
                     ⟶ (bound_d4 ∈ (happensBefore bound_c18))))))
               ∧ ¬(∃bound_c28.
                 (∃bound_anyArgs2.
                   ((((bound_c28 ∈ ({} ∪ snapshotAddition)) ∧ (bound_c18 ≠ bound_c28))
                     ∧ (((calls bound_c28) = (message_author_assign bound_m3 bound_anyArgs2))
                       ∧ (∀bound_d5.
                         (((bound_d5 ∈ ({} ∪ snapshotAddition)) ∧ ((calls bound_d5) = (message_delete bound_m3)))
                           ⟶ (bound_d5 ∈ (happensBefore bound_c28))))))
                     ∧ (bound_c18 ∈ (happensBefore bound_c28))))))))))"
         
assumes message_author_mv_contains_res_def_4:

        "(∀bound_m3.
         (∀bound_a21.
           (((message_author_mv_contains_res3 bound_m3) bound_a21)
             = (∃bound_c19.
             (((bound_c19 ∈ ({} ∪ snapshotAddition))
               ∧ (((calls bound_c19) = (message_author_assign bound_m3 bound_a21))
                 ∧ (∀bound_d6.
                   (((bound_d6 ∈ ({} ∪ snapshotAddition)) ∧ ((calls bound_d6) = (message_delete bound_m3)))
                     ⟶ (bound_d6 ∈ (happensBefore bound_c19))))))
               ∧ ¬(∃bound_c29.
                 (∃bound_anyArgs3.
                   ((((bound_c29 ∈ ({} ∪ snapshotAddition)) ∧ (bound_c19 ≠ bound_c29))
                     ∧ (((calls bound_c29) = (message_author_assign bound_m3 bound_anyArgs3))
                       ∧ (∀bound_d7.
                         (((bound_d7 ∈ ({} ∪ snapshotAddition)) ∧ ((calls bound_d7) = (message_delete bound_m3)))
                           ⟶ (bound_d7 ∈ (happensBefore bound_c29))))))
                     ∧ (bound_c19 ∈ (happensBefore bound_c29))))))))))"
         
assumes before_procedure_invocation_invariant_1:

        "((∀bound_m2.
         (∀bound_a1.
           (∀bound_a2.
             ((((message_exists_res bound_m2) ∧ ((message_author_mv_contains_res bound_m2) bound_a1))
               ∧ ((message_author_mv_contains_res1 bound_m2) bound_a2))
               ⟶ (bound_a1 = bound_a2)))))
         ∧ (∀bound_m3.
           (∀bound_a11.
             (∀bound_a21.
               ((((message_exists_res1 bound_m3) ∧ ((message_author_mv_contains_res2 bound_m3) bound_a11))
                 ∧ ((message_author_mv_contains_res3 bound_m3) bound_a21))
                 ⟶ (bound_a11 = bound_a21))))))"
         
assumes messages_contains_res_def:

        "(∀bound_m.
         ((messages_contains_res bound_m)
           = (∃bound_c1.
           (((bound_c1 ∈ snapshotAddition) ∧ ((calls bound_c1) = (messages_add bound_m)))
             ∧ (∀bound_c2.
               (((bound_c2 ∈ snapshotAddition) ∧ ((calls bound_c2) = (messages_remove bound_m)))
                 ⟶ (bound_c2 ∈ (happensBefore bound_c1))))))))"
         
assumes message_exists_res_def_3:

        "(∀bound_m.
         ((message_exists_res2 bound_m)
           = (∃bound_c11.
           ((((bound_c11 ∈ snapshotAddition) ∧ (∃bound_args. ((calls bound_c11) = (message_author_assign bound_m bound_args))))
             ∨ ((bound_c11 ∈ snapshotAddition)
               ∧ (∃bound_args1. ((calls bound_c11) = (message_content_assign bound_m bound_args1)))))
             ∧ (∀bound_c21.
               (((bound_c21 ∈ snapshotAddition) ∧ ((calls bound_c21) = (message_delete bound_m)))
                 ⟶ (bound_c21 ∈ (happensBefore bound_c11))))))))"
         
assumes messages_contains_res_def_2:

        "(∀bound_m1.
         ((messages_contains_res1 bound_m1)
           = (∃bound_c12.
           (((bound_c12 ∈ ({} ∪ snapshotAddition)) ∧ ((calls bound_c12) = (messages_add bound_m1)))
             ∧ (∀bound_c22.
               (((bound_c22 ∈ ({} ∪ snapshotAddition)) ∧ ((calls bound_c22) = (messages_remove bound_m1)))
                 ⟶ (bound_c22 ∈ (happensBefore bound_c12))))))))"
         
assumes message_exists_res_def_4:

        "(∀bound_m1.
         ((message_exists_res3 bound_m1)
           = (∃bound_c13.
           ((((bound_c13 ∈ ({} ∪ snapshotAddition))
             ∧ (∃bound_args2. ((calls bound_c13) = (message_author_assign bound_m1 bound_args2))))
             ∨ ((bound_c13 ∈ ({} ∪ snapshotAddition))
               ∧ (∃bound_args3. ((calls bound_c13) = (message_content_assign bound_m1 bound_args3)))))
             ∧ (∀bound_c23.
               (((bound_c23 ∈ ({} ∪ snapshotAddition)) ∧ ((calls bound_c23) = (message_delete bound_m1)))
                 ⟶ (bound_c23 ∈ (happensBefore bound_c13))))))))"
         
assumes before_procedure_invocation_invariant_0:

        "((∀bound_m. ((messages_contains_res bound_m) ⟶ (message_exists_res2 bound_m)))
         ∧ (∀bound_m1. ((messages_contains_res1 bound_m1) ⟶ (message_exists_res3 bound_m1))))"
         
assumes i_fresh:

        "((invocationOp currentInvocation) = no_invocation)"
         
assumes old_transactions_unchanged:

        "(∀c5.
         (∀tx3.
           (((((calls c5) = no_call) ∧ ((calls1 c5) ≠ no_call)) ∧ ((callOrigin1 c5) = (Some tx3)))
             ⟶ ((transactionOrigin tx3) = None))))"
         
assumes growth_invocation_res:

        "(∀i1. (((invocationRes i1) ≠ NoReturn) ⟶ ((invocationRes i1) = (invocationRes i1))))"
         
assumes growth_invocation_op:

        "(∀i.
         ((((invocationOp(currentInvocation := (sendMessage from_init content_init))) i) ≠ no_invocation)
           ⟶ (((invocationOp(currentInvocation := (sendMessage from_init content_init))) i)
             = ((invocationOp(currentInvocation := (sendMessage from_init content_init))) i))))"
         
assumes growth_tx_origin:

        "(∀tx2. (¬((transactionOrigin tx2) = None) ⟶ ((transactionOrigin1 tx2) = (transactionOrigin tx2))))"
         
assumes growth_call_tx:

        "(∀c4. (((calls c4) ≠ no_call) ⟶ ((callOrigin1 c4) = (callOrigin c4))))"
         
assumes growth_happensbefore:

        "(∀c3. (((calls c3) ≠ no_call) ⟶ ((happensBefore1 c3) = (happensBefore c3))))"
         
assumes growth_calls:

        "(∀c2. (((calls c2) ≠ no_call) ⟶ ((calls1 c2) = (calls c2))))"
         
assumes growth_visible_calls:

        "(∀c1. ((c1 ∈ {}) ⟶ (c1 ∈ {})))"
         
assumes growth_callOrigin:

        "(∀c. (∀tx1. (((callOrigin c) = (Some tx1)) ⟶ ((callOrigin1 c) = (Some tx1)))))"
         
assumes transaction_begin_snapshot_addition_transaction_consistent:

        "(∀bound_c143.
         (∀bound_c243.
           (((bound_c143 ∈ snapshotAddition1) ∧ (bound_c243 ∈ (happensBefore1 bound_c143))) ⟶ (bound_c243 ∈ snapshotAddition1))))"
         
assumes transaction_begin_snapshot_addition_transaction_consistent_2:

        "(∀bound_c142.
         (∀bound_c242.
           (((bound_c142 ∈ snapshotAddition1) ∧ ((callOrigin1 bound_c142) = (callOrigin1 bound_c242)))
             ⟶ (bound_c242 ∈ snapshotAddition1))))"
         
assumes transaction_begin_snapshot_addition_subset_calls:

        "(∀bound_c43. ((bound_c43 ∈ snapshotAddition1) ⟶ ((calls1 bound_c43) ≠ no_call)))"
         
assumes transaction_begin_MessageId_knownIds_are_generated:

        "(∀bound_x6. ((bound_x6 ∈ knownIds_MessageId1) ⟶ ¬((generatedIds_MessageId1 bound_x6) = None)))"
         
assumes transaction_begin_message_delete_call_parameter_key_generated:

        "(∀bound_c42.
         (∀bound_key5. (((calls1 bound_c42) = (message_delete bound_key5)) ⟶ ¬((generatedIds_MessageId1 bound_key5) = None))))"
         
assumes transaction_begin_message_content_assign_call_parameter_key_generated:

        "(∀bound_c41.
         (∀bound_key4.
           (∀bound_value3.
             (((calls1 bound_c41) = (message_content_assign bound_key4 bound_value3))
               ⟶ ¬((generatedIds_MessageId1 bound_key4) = None)))))"
         
assumes transaction_begin_message_author_assign_call_parameter_key_generated:

        "(∀bound_c40.
         (∀bound_key3.
           (∀bound_value2.
             (((calls1 bound_c40) = (message_author_assign bound_key3 bound_value2))
               ⟶ ¬((generatedIds_MessageId1 bound_key3) = None)))))"
         
assumes transaction_begin_messages_remove_call_parameter_elem_generated:

        "(∀bound_c39.
         (∀bound_elem3.
           (((calls1 bound_c39) = (messages_remove bound_elem3)) ⟶ ¬((generatedIds_MessageId1 bound_elem3) = None))))"
         
assumes transaction_begin_messages_add_call_parameter_elem_generated:

        "(∀bound_c38.
         (∀bound_elem2. (((calls1 bound_c38) = (messages_add bound_elem2)) ⟶ ¬((generatedIds_MessageId1 bound_elem2) = None))))"
         
assumes transaction_begin_sendMessage_result_known:

        "(∀bound_i9.
         (∀bound_result1.
           (((invocationRes bound_i9) = (sendMessage_res bound_result1)) ⟶ (bound_result1 ∈ knownIds_MessageId1))))"
         
assumes transaction_begin_WF_transactionOrigin_exists:

        "(∀bound_tx10.
         (∀bound_i8.
           (((transactionOrigin1 bound_tx10) = (Some bound_i8))
             ⟶ (((invocationOp(currentInvocation := (sendMessage from_init content_init))) bound_i8) ≠ no_invocation))))"
         
assumes transaction_begin_WF_callOrigin_exists:

        "(∀bound_ca3. (∀bound_tx9. (((callOrigin1 bound_ca3) = (Some bound_tx9)) ⟶ ¬((transactionOrigin1 bound_tx9) = None))))"
         
assumes transaction_begin_WF_no_call_implies_not_in_happensBefore:

        "(∀bound_ca2. (∀bound_cb1. (((callOrigin1 bound_ca2) = None) ⟶ ¬(bound_ca2 ∈ (happensBefore1 bound_cb1)))))"
         
assumes transaction_begin_WF_no_call_implies_no_happensBefore:

        "(∀bound_c37. (((callOrigin1 bound_c37) = None) ⟶ ((happensBefore1 bound_c37) = {})))"
         
assumes transaction_begin_WF_transactionOrigin_callOrigin:

        "(∀bound_tx8. (((transactionOrigin1 bound_tx8) = None) ⟶ (∀bound_c36. ((callOrigin1 bound_c36) ≠ (Some bound_tx8)))))"
         
assumes transaction_begin_WF_callOrigin:

        "(∀bound_c35. (((callOrigin1 bound_c35) = None) = ((calls1 bound_c35) = no_call)))"
         
assumes transaction_begin_WF_invocationCalls:

        "(∀bound_i7.
         (∀bound_c34.
           ((bound_c34 ∈ (invocationCalls1 bound_i7))
             = (∃bound_tx7. (((callOrigin1 bound_c34) = (Some bound_tx7)) ∧ ((transactionOrigin1 bound_tx7) = (Some bound_i7)))))))"
         
assumes transaction_begin_no_invocation_implies_no_result:

        "(∀bound_x12.
         (∀bound_x21.
           (∀bound_y12.
             (∀bound_y22.
               ((((((callOrigin1 bound_x12) = (callOrigin1 bound_x21)) ∧ ((callOrigin1 bound_y12) = (callOrigin1 bound_y22)))
                 ∧ ¬((callOrigin1 bound_x12) = (callOrigin1 bound_y12)))
                 ∧ (bound_x21 ∈ (happensBefore1 bound_y12)))
                 ⟶ (bound_x21 ∈ (happensBefore1 bound_y22)))))))"
         
assumes transaction_begin_no_invocation_implies_no_result_2:

        "(∀bound_i6.
         ((((invocationOp(currentInvocation := (sendMessage from_init content_init))) bound_i6) = no_invocation)
           ⟶ ((invocationRes bound_i6) = NoReturn)))"
         
assumes transaction_begin_happensBefore_antisym:

        "(∀bound_x5.
         (∀bound_y4.
           (((bound_x5 ∈ (happensBefore1 bound_y4)) ∧ (bound_y4 ∈ (happensBefore1 bound_x5))) ⟶ (bound_x5 = bound_y4))))"
         
assumes transaction_begin_happensBefore_trans:

        "(∀bound_x4.
         (∀bound_y3.
           (∀bound_z1.
             (((bound_x4 ∈ (happensBefore1 bound_y3)) ∧ (bound_y3 ∈ (happensBefore1 bound_z1)))
               ⟶ (bound_x4 ∈ (happensBefore1 bound_y3))))))"
         
assumes transaction_begin_happensBefore_reflex:

        "(∀bound_c33. (((calls1 bound_c33) ≠ no_call) ⟶ (bound_c33 ∈ (happensBefore1 bound_c33))))"
         
assumes transaction_begin_visibleCalls_causally_consistent:

        "(∀bound_c141. (∀bound_c241. (((bound_c241 ∈ vis) ∧ (bound_c141 ∈ (happensBefore1 bound_c241))) ⟶ (bound_c141 ∈ vis))))"
         
assumes transaction_begin_visibleCalls_transaction_consistent1:

        "(∀bound_c140.
         (∀bound_c240.
           ((((bound_c140 ∈ vis) ∧ ((callOrigin1 bound_c140) = (callOrigin1 bound_c240))) ∧ ((calls1 bound_c240) ≠ no_call))
             ⟶ (bound_c240 ∈ vis))))"
         
assumes transaction_begin_visibleCalls_exist:

        "(∀bound_c32. ((bound_c32 ∈ vis) ⟶ ((calls1 bound_c32) ≠ no_call)))"
         
assumes transaction_begin_invocation_sequential:

        "(∀bound_c139.
         (∀bound_tx11.
           (∀bound_i5.
             (∀bound_c239.
               (∀bound_tx21.
                 ((((((callOrigin1 bound_c139) = (Some bound_tx11)) ∧ ((transactionOrigin1 bound_tx11) = (Some bound_i5)))
                   ∧ ((callOrigin1 bound_c239) = (Some bound_tx21)))
                   ∧ ((transactionOrigin1 bound_tx21) = (Some bound_i5)))
                   ⟶ ((bound_c139 ∈ (happensBefore1 bound_c239)) ∨ (bound_c239 ∈ (happensBefore1 bound_c139)))))))))"
         
assumes transaction_begin_happensBefore_exists_r:

        "(∀bound_c138. (∀bound_c238. (((calls1 bound_c138) = no_call) ⟶ ¬(bound_c138 ∈ (happensBefore1 bound_c238)))))"
         
assumes transaction_begin_happensBefore_exists_l:

        "(∀bound_c137. (∀bound_c237. (((calls1 bound_c137) = no_call) ⟶ ¬(bound_c137 ∈ (happensBefore1 bound_c237)))))"
         
assumes message_exists_res_def_5:

        "(∀bound_m10.
         ((message_exists_res4 bound_m10)
           = (∃bound_c131.
           ((((bound_c131 ∈ snapshotAddition1)
             ∧ (∃bound_args20. ((calls1 bound_c131) = (message_author_assign bound_m10 bound_args20))))
             ∨ ((bound_c131 ∈ snapshotAddition1)
               ∧ (∃bound_args21. ((calls1 bound_c131) = (message_content_assign bound_m10 bound_args21)))))
             ∧ (∀bound_c231.
               (((bound_c231 ∈ snapshotAddition1) ∧ ((calls1 bound_c231) = (message_delete bound_m10)))
                 ⟶ (bound_c231 ∈ (happensBefore1 bound_c131))))))))"
         
assumes message_author_mv_contains_res_def_5:

        "(∀bound_m10.
         (∀bound_a14.
           (((message_author_mv_contains_res4 bound_m10) bound_a14)
             = (∃bound_c132.
             (((bound_c132 ∈ snapshotAddition1)
               ∧ (((calls1 bound_c132) = (message_author_assign bound_m10 bound_a14))
                 ∧ (∀bound_d16.
                   (((bound_d16 ∈ snapshotAddition1) ∧ ((calls1 bound_d16) = (message_delete bound_m10)))
                     ⟶ (bound_d16 ∈ (happensBefore1 bound_c132))))))
               ∧ ¬(∃bound_c232.
                 (∃bound_anyArgs8.
                   ((((bound_c232 ∈ snapshotAddition1) ∧ (bound_c132 ≠ bound_c232))
                     ∧ (((calls1 bound_c232) = (message_author_assign bound_m10 bound_anyArgs8))
                       ∧ (∀bound_d17.
                         (((bound_d17 ∈ snapshotAddition1) ∧ ((calls1 bound_d17) = (message_delete bound_m10)))
                           ⟶ (bound_d17 ∈ (happensBefore1 bound_c232))))))
                     ∧ (bound_c132 ∈ (happensBefore1 bound_c232))))))))))"
         
assumes message_author_mv_contains_res_def_6:

        "(∀bound_m10.
         (∀bound_a24.
           (((message_author_mv_contains_res5 bound_m10) bound_a24)
             = (∃bound_c133.
             (((bound_c133 ∈ snapshotAddition1)
               ∧ (((calls1 bound_c133) = (message_author_assign bound_m10 bound_a24))
                 ∧ (∀bound_d18.
                   (((bound_d18 ∈ snapshotAddition1) ∧ ((calls1 bound_d18) = (message_delete bound_m10)))
                     ⟶ (bound_d18 ∈ (happensBefore1 bound_c133))))))
               ∧ ¬(∃bound_c233.
                 (∃bound_anyArgs9.
                   ((((bound_c233 ∈ snapshotAddition1) ∧ (bound_c133 ≠ bound_c233))
                     ∧ (((calls1 bound_c233) = (message_author_assign bound_m10 bound_anyArgs9))
                       ∧ (∀bound_d19.
                         (((bound_d19 ∈ snapshotAddition1) ∧ ((calls1 bound_d19) = (message_delete bound_m10)))
                           ⟶ (bound_d19 ∈ (happensBefore1 bound_c233))))))
                     ∧ (bound_c133 ∈ (happensBefore1 bound_c233))))))))))"
         
assumes message_exists_res_def_6:

        "(∀bound_m11.
         ((message_exists_res5 bound_m11)
           = (∃bound_c134.
           ((((bound_c134 ∈ (vis ∪ snapshotAddition1))
             ∧ (∃bound_args22. ((calls1 bound_c134) = (message_author_assign bound_m11 bound_args22))))
             ∨ ((bound_c134 ∈ (vis ∪ snapshotAddition1))
               ∧ (∃bound_args23. ((calls1 bound_c134) = (message_content_assign bound_m11 bound_args23)))))
             ∧ (∀bound_c234.
               (((bound_c234 ∈ (vis ∪ snapshotAddition1)) ∧ ((calls1 bound_c234) = (message_delete bound_m11)))
                 ⟶ (bound_c234 ∈ (happensBefore1 bound_c134))))))))"
         
assumes message_author_mv_contains_res_def_7:

        "(∀bound_m11.
         (∀bound_a15.
           (((message_author_mv_contains_res6 bound_m11) bound_a15)
             = (∃bound_c135.
             (((bound_c135 ∈ (vis ∪ snapshotAddition1))
               ∧ (((calls1 bound_c135) = (message_author_assign bound_m11 bound_a15))
                 ∧ (∀bound_d20.
                   (((bound_d20 ∈ (vis ∪ snapshotAddition1)) ∧ ((calls1 bound_d20) = (message_delete bound_m11)))
                     ⟶ (bound_d20 ∈ (happensBefore1 bound_c135))))))
               ∧ ¬(∃bound_c235.
                 (∃bound_anyArgs10.
                   ((((bound_c235 ∈ (vis ∪ snapshotAddition1)) ∧ (bound_c135 ≠ bound_c235))
                     ∧ (((calls1 bound_c235) = (message_author_assign bound_m11 bound_anyArgs10))
                       ∧ (∀bound_d21.
                         (((bound_d21 ∈ (vis ∪ snapshotAddition1)) ∧ ((calls1 bound_d21) = (message_delete bound_m11)))
                           ⟶ (bound_d21 ∈ (happensBefore1 bound_c235))))))
                     ∧ (bound_c135 ∈ (happensBefore1 bound_c235))))))))))"
         
assumes message_author_mv_contains_res_def_8:

        "(∀bound_m11.
         (∀bound_a25.
           (((message_author_mv_contains_res7 bound_m11) bound_a25)
             = (∃bound_c136.
             (((bound_c136 ∈ (vis ∪ snapshotAddition1))
               ∧ (((calls1 bound_c136) = (message_author_assign bound_m11 bound_a25))
                 ∧ (∀bound_d22.
                   (((bound_d22 ∈ (vis ∪ snapshotAddition1)) ∧ ((calls1 bound_d22) = (message_delete bound_m11)))
                     ⟶ (bound_d22 ∈ (happensBefore1 bound_c136))))))
               ∧ ¬(∃bound_c236.
                 (∃bound_anyArgs11.
                   ((((bound_c236 ∈ (vis ∪ snapshotAddition1)) ∧ (bound_c136 ≠ bound_c236))
                     ∧ (((calls1 bound_c236) = (message_author_assign bound_m11 bound_anyArgs11))
                       ∧ (∀bound_d23.
                         (((bound_d23 ∈ (vis ∪ snapshotAddition1)) ∧ ((calls1 bound_d23) = (message_delete bound_m11)))
                           ⟶ (bound_d23 ∈ (happensBefore1 bound_c236))))))
                     ∧ (bound_c136 ∈ (happensBefore1 bound_c236))))))))))"
         
assumes at_transaction_begin_invariant_1:

        "((∀bound_m10.
         (∀bound_a14.
           (∀bound_a24.
             ((((message_exists_res4 bound_m10) ∧ ((message_author_mv_contains_res4 bound_m10) bound_a14))
               ∧ ((message_author_mv_contains_res5 bound_m10) bound_a24))
               ⟶ (bound_a14 = bound_a24)))))
         ∧ (∀bound_m11.
           (∀bound_a15.
             (∀bound_a25.
               ((((message_exists_res5 bound_m11) ∧ ((message_author_mv_contains_res6 bound_m11) bound_a15))
                 ∧ ((message_author_mv_contains_res7 bound_m11) bound_a25))
                 ⟶ (bound_a15 = bound_a25))))))"
         
assumes messages_contains_res_def_3:

        "(∀bound_m8.
         ((messages_contains_res2 bound_m8)
           = (∃bound_c127.
           (((bound_c127 ∈ snapshotAddition1) ∧ ((calls1 bound_c127) = (messages_add bound_m8)))
             ∧ (∀bound_c227.
               (((bound_c227 ∈ snapshotAddition1) ∧ ((calls1 bound_c227) = (messages_remove bound_m8)))
                 ⟶ (bound_c227 ∈ (happensBefore1 bound_c127))))))))"
         
assumes message_exists_res_def_7:

        "(∀bound_m8.
         ((message_exists_res6 bound_m8)
           = (∃bound_c128.
           ((((bound_c128 ∈ snapshotAddition1)
             ∧ (∃bound_args16. ((calls1 bound_c128) = (message_author_assign bound_m8 bound_args16))))
             ∨ ((bound_c128 ∈ snapshotAddition1)
               ∧ (∃bound_args17. ((calls1 bound_c128) = (message_content_assign bound_m8 bound_args17)))))
             ∧ (∀bound_c228.
               (((bound_c228 ∈ snapshotAddition1) ∧ ((calls1 bound_c228) = (message_delete bound_m8)))
                 ⟶ (bound_c228 ∈ (happensBefore1 bound_c128))))))))"
         
assumes messages_contains_res_def_4:

        "(∀bound_m9.
         ((messages_contains_res3 bound_m9)
           = (∃bound_c129.
           (((bound_c129 ∈ (vis ∪ snapshotAddition1)) ∧ ((calls1 bound_c129) = (messages_add bound_m9)))
             ∧ (∀bound_c229.
               (((bound_c229 ∈ (vis ∪ snapshotAddition1)) ∧ ((calls1 bound_c229) = (messages_remove bound_m9)))
                 ⟶ (bound_c229 ∈ (happensBefore1 bound_c129))))))))"
         
assumes message_exists_res_def_8:

        "(∀bound_m9.
         ((message_exists_res7 bound_m9)
           = (∃bound_c130.
           ((((bound_c130 ∈ (vis ∪ snapshotAddition1))
             ∧ (∃bound_args18. ((calls1 bound_c130) = (message_author_assign bound_m9 bound_args18))))
             ∨ ((bound_c130 ∈ (vis ∪ snapshotAddition1))
               ∧ (∃bound_args19. ((calls1 bound_c130) = (message_content_assign bound_m9 bound_args19)))))
             ∧ (∀bound_c230.
               (((bound_c230 ∈ (vis ∪ snapshotAddition1)) ∧ ((calls1 bound_c230) = (message_delete bound_m9)))
                 ⟶ (bound_c230 ∈ (happensBefore1 bound_c130))))))))"
         
assumes at_transaction_begin_invariant_0:

        "((∀bound_m8. ((messages_contains_res2 bound_m8) ⟶ (message_exists_res6 bound_m8)))
         ∧ (∀bound_m9. ((messages_contains_res3 bound_m9) ⟶ (message_exists_res7 bound_m9))))"
         
assumes no_new_calls_addded_to_current:

        "((invocationCalls currentInvocation) = (invocationCalls1 currentInvocation))"
         
assumes no_new_transactions_added_to_current:

        "(∀bound_t.
         (((transactionOrigin bound_t) = (Some currentInvocation)) = ((transactionOrigin1 bound_t) = (Some currentInvocation))))"
         
assumes tx_fresh:

        "((transactionOrigin1 tx) = None)"
         
assumes vis_update:

        "(vis = ({} ∪ newCalls))"
         
assumes new_transactions_exist:

        "(newTxns ⊆ (dom transactionOrigin1))"
         
assumes m_new_id_fresh:

        "((generatedIds_MessageId1 m) = None)"
         
assumes c0_freshB:

        "((calls1 c0) = no_call)"
         
assumes c0_freshA:

        "distinct [c0]"
         
assumes calls_def:

        "(calls2 = (calls1(c0 := (message_author_assign m from_init))))"
         
assumes c11_freshB:

        "((calls2 c11) = no_call)"
         
assumes c11_freshA:

        "distinct [c11 , c0]"
         
assumes calls_def_2:

        "(calls3 = (calls2(c11 := (message_content_assign m content_init))))"
         
assumes c21_freshB:

        "((calls3 c21) = no_call)"
         
assumes c21_freshA:

        "distinct [c21 , c0 , c11]"
         
assumes calls_def_3:

        "(calls4 = (calls3(c21 := (messages_add m))))"
         
assumes vis_def:

        "(vis1 = (vis ∪ {c0}))"
         
assumes vis_def_2:

        "(vis2 = (vis1 ∪ {c11}))"
         
assumes happensBefore_def:

        "(happensBefore2 = (happensBefore1(c0 := (vis ∪ {c0}))))"
         
assumes happensBefore_def_2:

        "(happensBefore3 = (happensBefore2(c11 := (vis1 ∪ {c11}))))"
         
assumes happensBefore_def_3:

        "(happensBefore4 = (happensBefore3(c21 := (vis2 ∪ {c21}))))"
         
assumes messages_contains_res_def_5:

        "(∀bound_m12.
         ((messages_contains_res4 bound_m12)
           = (∃bound_c151.
           (((bound_c151 ∈ snapshotAddition1) ∧ ((calls4 bound_c151) = (messages_add bound_m12)))
             ∧ (∀bound_c251.
               (((bound_c251 ∈ snapshotAddition1) ∧ ((calls4 bound_c251) = (messages_remove bound_m12)))
                 ⟶ (bound_c251 ∈ (happensBefore4 bound_c151))))))))"
         
assumes message_exists_res_def_9:

        "(∀bound_m12.
         ((message_exists_res8 bound_m12)
           = (∃bound_c152.
           ((((bound_c152 ∈ snapshotAddition1)
             ∧ (∃bound_args24. ((calls4 bound_c152) = (message_author_assign bound_m12 bound_args24))))
             ∨ ((bound_c152 ∈ snapshotAddition1)
               ∧ (∃bound_args25. ((calls4 bound_c152) = (message_content_assign bound_m12 bound_args25)))))
             ∧ (∀bound_c252.
               (((bound_c252 ∈ snapshotAddition1) ∧ ((calls4 bound_c252) = (message_delete bound_m12)))
                 ⟶ (bound_c252 ∈ (happensBefore4 bound_c152))))))))"
         
assumes vis_def_3:

        "(vis3 = (vis2 ∪ {c21}))"
         
assumes messages_contains_res_def_6:

        "(∀bound_m13.
         ((messages_contains_res5 bound_m13)
           = (∃bound_c153.
           (((bound_c153 ∈ (vis3 ∪ snapshotAddition1)) ∧ ((calls4 bound_c153) = (messages_add bound_m13)))
             ∧ (∀bound_c253.
               (((bound_c253 ∈ (vis3 ∪ snapshotAddition1)) ∧ ((calls4 bound_c253) = (messages_remove bound_m13)))
                 ⟶ (bound_c253 ∈ (happensBefore4 bound_c153))))))))"
         
assumes message_exists_res_def_10:

        "(∀bound_m13.
         ((message_exists_res9 bound_m13)
           = (∃bound_c154.
           ((((bound_c154 ∈ (vis3 ∪ snapshotAddition1))
             ∧ (∃bound_args26. ((calls4 bound_c154) = (message_author_assign bound_m13 bound_args26))))
             ∨ ((bound_c154 ∈ (vis3 ∪ snapshotAddition1))
               ∧ (∃bound_args27. ((calls4 bound_c154) = (message_content_assign bound_m13 bound_args27)))))
             ∧ (∀bound_c254.
               (((bound_c254 ∈ (vis3 ∪ snapshotAddition1)) ∧ ((calls4 bound_c254) = (message_delete bound_m13)))
                 ⟶ (bound_c254 ∈ (happensBefore4 bound_c154))))))))"
         
assumes invariant_not_violated:

        "¬((∀bound_m12. ((messages_contains_res4 bound_m12) ⟶ (message_exists_res8 bound_m12)))
         ∧ (∀bound_m13. ((messages_contains_res5 bound_m13) ⟶ (message_exists_res9 bound_m13))))"
         shows False

  using invariant_not_violated
proof (rule notE)
  have "c0 ∉ snapshotAddition1"
    using c0_freshB transaction_begin_snapshot_addition_subset_calls by auto
  have "c11 ∉ snapshotAddition1"
    using c11_freshA c11_freshB calls_def transaction_begin_snapshot_addition_subset_calls by auto
  have "c21 ∉ snapshotAddition1"
    using c21_freshA c21_freshB calls_def calls_def_2 transaction_begin_snapshot_addition_subset_calls by auto


  have s0[simp]:"x ≠ c0" if "x ∈ snapshotAddition1" for x
    using ‹c0 ∉ snapshotAddition1› that by blast
  have s2[simp]:"x ≠ c11" if "x ∈ snapshotAddition1" for x
    using ‹c11 ∉ snapshotAddition1› that by blast
  have s2[simp]:"x ≠ c21" if "x ∈ snapshotAddition1" for x
    using ‹c21 ∉ snapshotAddition1› that by blast


  from c21_freshA have [simp]: "c0 ≠ c11" by simp
  from c21_freshA have [simp]: "c0 ≠ c21"  by auto 
  from c21_freshA have [simp]: "c11 ≠ c21" by auto

  have [simp]: "c0 ∉ vis"
    using c0_freshB transaction_begin_visibleCalls_exist by blast
  have [simp]: "c21 ∉ vis"
    by (metis ‹c0 ≠ c21› ‹c11 ≠ c21› c21_freshB calls_def calls_def_2 fun_upd_apply transaction_begin_visibleCalls_exist)

  have [simp]: "c11 ∉ vis"
    by (metis ‹c0 ≠ c11› c11_freshB calls_def fun_upd_apply transaction_begin_visibleCalls_exist)

  have  [simp]: "c31 ∉ vis"
    by (metis ‹c11 ≠ c31› ‹c21 ≠ c31› ‹c31 ≠ c0› assms(102) assms(105) assms(108) assms(109) assms(78) fun_upd_apply)


  show "(∀bound_c65 bound_m12. chat_messages_contains_res4 bound_c65 bound_m12 ⟶ message_exists_res8 bound_m12) ∧
    (∀bound_c66 bound_m13. chat_messages_contains_res5 bound_c66 bound_m13 ⟶ message_exists_res9 bound_m13)"
  proof (intro allI conjI impI)
    fix c m
    assume a1: "chat_messages_contains_res4 c m"



    from a1 have "chat_messages_contains_res2 c m"
      apply (auto simp add: chat_messages_contains_res_def_5 chat_messages_contains_res_def_3)
      apply (auto simp add: calls_def_4 calls_def_3 calls_def_2 calls_def cong: conj_cong)
      apply (auto simp add:  happensBefore_def_4 happensBefore_def_3 happensBefore_def_2 happensBefore_def )
      done

    find_theorems " happensBefore5 "

    find_theorems calls2

    have "chat_messages_contains_res2 c m ⟶ message_exists_res6 m"
      by (simp add: at_transaction_begin_invariant_0)

    hence "message_exists_res6 m"
      using ‹chat_messages_contains_res2 c m› by blast

    from `message_exists_res6 m` 
    show  "message_exists_res8 m"
      apply (auto simp add: message_exists_res_def_7 message_exists_res_def_9)
        apply (auto simp add:  calls_def_4 calls_def_3 calls_def_2 calls_def  happensBefore_def_4 happensBefore_def_3 happensBefore_def_2 happensBefore_def cong: conj_cong)
      using ‹c11 ∉ snapshotAddition1› s0 s2 s3 apply blast
      using ‹c0 ∉ snapshotAddition1› ‹c11 ∉ snapshotAddition1› ‹c21 ∉ snapshotAddition1› ‹c31 ∉ snapshotAddition1› apply auto[1]
      using ‹c11 ∉ snapshotAddition1› s0 s2 s3 by blast
  next
    fix c m
    assume a1: "chat_messages_contains_res5 c m"

    from a1 obtain cm
      where c1: "(cm ∈ vis4 ∨ cm ∈ snapshotAddition1)"
        and c2: "calls5 cm = chat_messages_add c m"
        and c3: "(∀bound_d38. (bound_d38 ∈ vis4 ∨ bound_d38 ∈ snapshotAddition1) ∧ calls5 bound_d38 = chat_delete c ⟶ bound_d38 ∈ happensBefore5 cm)"
        and c4: "
       (∀bound_c253.
           (bound_c253 ∈ vis4 ∨ bound_c253 ∈ snapshotAddition1) ∧
           calls5 bound_c253 = chat_messages_remove c m ∧
           (∀bound_d39. (bound_d39 ∈ vis4 ∨ bound_d39 ∈ snapshotAddition1) ∧ calls5 bound_d39 = chat_delete c ⟶ bound_d39 ∈ happensBefore5 bound_c253) ⟶
           bound_c253 ∈ happensBefore5 cm)"
      by (auto simp add: chat_messages_contains_res_6)

    show " message_exists_res9 m"
    proof (cases "cm ∈ snapshotAddition1")
      case True
      from a1 c2 c3 c4 have "chat_messages_contains_res3 c m"
        apply (auto simp add: chat_messages_contains_res_6 chat_messages_contains_res_4)
         apply (auto simp add: calls_4 calls_3 calls_2 calls happensBefore_4 happensBefore_3 happensBefore_2 happensBefore  cong: conj_cong)
        using True ‹c31 ∉ snapshotAddition1›  apply (auto simp add: split: if_splits)
        using m_new_id_fresh transaction_begin_chat_messages_add_call_parameter_elem_generated apply auto[1]
         apply (rule_tac x=bound_c153 in exI)
         apply (auto simp add:  vis vis_2 vis_3 vis_4)
        using ‹c11 ∉ snapshotAddition1› ‹c21 ∉ snapshotAddition1› c0_freshB transaction_begin_snapshot_addition_subset_calls apply blast
         apply (smt ‹c21 ∉ snapshotAddition1› c0_freshB c11_freshB calls fun_upd_apply transaction_begin_snapshot_addition_subset_calls)

        apply (rule_tac x=bound_c153 in exI)
        apply (auto simp add:  vis vis_2 vis_3 vis_4)
         apply (metis ‹c11 ∉ snapshotAddition1› s0 s2)
        by (metis ‹c21 ∉ snapshotAddition1› c0_freshB c11_freshB calls fun_upd_apply transaction_begin_snapshot_addition_subset_calls)
      also have "chat_messages_contains_res3 c m ⟶ message_exists_res7 m"
        by (simp add: at_transaction_begin_invariant_0)
      ultimately have "message_exists_res7 m"
        by simp
      thus "message_exists_res9 m"
        apply (auto simp add: message_exists_res_10 message_exists_res_8)
             apply (auto simp add:  calls_4 calls_3 calls_2 calls  happensBefore_4 happensBefore_3 happensBefore_2 happensBefore vis vis_2 vis_3 vis_4 cong: conj_cong)
             apply (smt c0_freshB c11_freshB c21_freshB c31_freshB calls calls_2 calls_3 fun_upd_apply transaction_begin_visibleCalls_exist)
            apply (smt ‹c11 ∉ snapshotAddition1› ‹c21 ∉ snapshotAddition1› ‹c31 ∉ snapshotAddition1› c0_freshB transaction_begin_snapshot_addition_subset_calls)
           apply (smt c0_freshB c11_freshB c21_freshB c31_freshB calls calls_2 calls_3 fun_upd_apply transaction_begin_visibleCalls_exist)
          apply (smt ‹c11 ∉ snapshotAddition1› ‹c21 ∉ snapshotAddition1› ‹c31 ∉ snapshotAddition1› c0_freshB transaction_begin_snapshot_addition_subset_calls)
         apply (smt c0_freshB c11_freshB c21_freshB c31_freshB calls calls_2 calls_3 fun_upd_apply transaction_begin_visibleCalls_exist)
        by (smt ‹c11 ∉ snapshotAddition1› ‹c21 ∉ snapshotAddition1› ‹c31 ∉ snapshotAddition1› c0_freshB transaction_begin_snapshot_addition_subset_calls)

    next
      case False
      hence "cm ∉ snapshotAddition1" .
      hence "cm ∈ vis4"
        using c1 by auto
      show "message_exists_res9 m"
      proof (cases "cm ∈ vis")
        case True



        from a1 c2 c3 c4 have "chat_messages_contains_res3 c m"
          apply (auto simp add: chat_messages_contains_res_6 chat_messages_contains_res_4)
           apply (auto simp add: calls_4 calls_3 calls_2 calls happensBefore_4 happensBefore_3 happensBefore_2 happensBefore  cong: conj_cong)
          using True ‹c31 ∉ snapshotAddition1›  apply (auto simp add: split: if_splits)
          using m_new_id_fresh transaction_begin_chat_messages_add_call_parameter_elem_generated apply auto[1]
           apply (rule_tac x=bound_c153 in exI)
           apply (auto simp add:  vis vis_2 vis_3 vis_4)
          using ‹c11 ∉ snapshotAddition1› ‹c21 ∉ snapshotAddition1› c0_freshB transaction_begin_snapshot_addition_subset_calls apply blast
           apply (smt ‹c21 ∉ snapshotAddition1› c0_freshB c11_freshB calls fun_upd_apply transaction_begin_snapshot_addition_subset_calls)

          apply (rule_tac x=bound_c153 in exI)
          apply (auto simp add:  vis vis_2 vis_3 vis_4)
           apply (metis ‹c11 ∉ snapshotAddition1› s0 s2)
          by (metis ‹c21 ∉ snapshotAddition1› c0_freshB c11_freshB calls fun_upd_apply transaction_begin_snapshot_addition_subset_calls)
        also have "chat_messages_contains_res3 c m ⟶ message_exists_res7 m"
          by (simp add: at_transaction_begin_invariant_0)
        ultimately have "message_exists_res7 m"
          by simp
        thus "message_exists_res9 m"
          apply (auto simp add: message_exists_res_10 message_exists_res_8)
               apply (auto simp add:  calls_4 calls_3 calls_2 calls  happensBefore_4 happensBefore_3 happensBefore_2 happensBefore vis vis_2 vis_3 vis_4 cong: conj_cong)
               apply (smt c0_freshB c11_freshB c21_freshB c31_freshB calls calls_2 calls_3 fun_upd_apply transaction_begin_visibleCalls_exist)
              apply (smt ‹c11 ∉ snapshotAddition1› ‹c21 ∉ snapshotAddition1› ‹c31 ∉ snapshotAddition1› c0_freshB transaction_begin_snapshot_addition_subset_calls)
             apply (smt c0_freshB c11_freshB c21_freshB c31_freshB calls calls_2 calls_3 fun_upd_apply transaction_begin_visibleCalls_exist)
            apply (smt ‹c11 ∉ snapshotAddition1› ‹c21 ∉ snapshotAddition1› ‹c31 ∉ snapshotAddition1› c0_freshB transaction_begin_snapshot_addition_subset_calls)
           apply (smt c0_freshB c11_freshB c21_freshB c31_freshB calls calls_2 calls_3 fun_upd_apply transaction_begin_visibleCalls_exist)
          by (smt ‹c11 ∉ snapshotAddition1› ‹c21 ∉ snapshotAddition1› ‹c31 ∉ snapshotAddition1› c0_freshB transaction_begin_snapshot_addition_subset_calls)

      next
        case False
        with c2 c3 c4 show ?thesis
          apply (auto simp add: message_exists_res_10)
          apply (rule_tac x=c21 in exI)
          apply (auto simp add: )
                   apply (auto simp add: vis_3 vis_4)[7]
          using ‹cm ∉ snapshotAddition1› c1 calls calls_2 calls_3 calls_4 vis vis_2 vis_3 vis_4 apply force
          using calls_4 happensBefore_3 happensBefore_4 vis_3 vis_4 apply force
          using ‹cm ∉ snapshotAddition1› c0_freshB c1 c11_freshB callInfo.distinct(143) callInfo.distinct(159) calls calls_2 calls_3 calls_4 fun_upd_apply m_new_id_fresh transaction_begin_message_delete_call_parameter_key_generated vis vis_2 vis_3 vis_4 by fastforce
      qed
    qed
  qed
qed

