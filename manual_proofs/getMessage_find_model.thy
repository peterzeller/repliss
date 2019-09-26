
theory "getMessage_find_model"
  imports Main
begin

      datatype CallId = CallId nat

datatype TxId = TxId nat

datatype MessageId = MessageId nat

datatype UserId = UserId nat

datatype String = String nat

datatype callInfo =
    message_delete (key2: "MessageId")
  | no_call 
  | message_author_assign (key: "MessageId") (qvalue: "UserId")
  | queryop_message_content_mv_contains (key7: "MessageId") (elem3: "String") (result5: "bool")
  | queryop_chat_contains (elem2: "MessageId") (result: "bool")
  | chat_add (elem: "MessageId")
  | message_content_assign (key1: "MessageId") (value1: "String")
  | chat_remove (elem1: "MessageId")
  | queryop_message_content_get (key5: "MessageId") (result3: "String")
  | queryop_message_author_isEqualTo (key4: "MessageId") (other: "UserId") (result2: "bool")
  | queryop_message_exists (key8: "MessageId") (result6: "bool")
  | queryop_message_content_getFirst (key6: "MessageId") (result4: "String")
  | queryop_message_author_get (key3: "MessageId") (result1: "UserId")

datatype InvocationId = InvocationId nat

datatype getMessageResult =
    notFound 
  | found (author: "UserId") (content1: "String")

datatype invocationResult =
    getMessage_res (getMessage_res_arg: "getMessageResult")
  | deleteMessage_res 
  | sendMessage_res (sendMessage_res_arg: "MessageId")
  | editMessage_res 
  | NoResult 

datatype invocationInfo =
    sendMessage (qfrom: "UserId") (content: "String")
  | getMessage (m: "MessageId")
  | editMessage (id: "MessageId") (newContent: "String")
  | deleteMessage (message_id: "MessageId")
  | no_invocation 

lemma "getMessage_line33":
fixes vis1 :: "CallId set"
         
fixes newCalls :: "CallId set"
         
fixes g1 :: "InvocationId"
         
fixes snapshotAddition :: "CallId set"
         
fixes q__query_message_exists_res_1 :: "bool"
         
fixes chat_contains_res :: "MessageId => bool"
         
fixes q__query_message_content_getFirst_res_3 :: "String"
         
fixes happensBefore3 :: "CallId => CallId set"
         
fixes knownIds_MessageId1 :: "MessageId set"
         
fixes for_snapshot_additions :: "bool"
         
fixes transactionOrigin1 :: "TxId => InvocationId option"
         
fixes c11 :: "CallId"
         
fixes tx1 :: "TxId"
         
fixes invocationRes :: "InvocationId => invocationResult"
         
fixes message_exists_res4 :: "bool"
         
fixes calls1 :: "CallId => callInfo"
         
fixes content4 :: "String"
         
fixes invocationOp :: "InvocationId => invocationInfo"
         
fixes callOrigin :: "CallId => TxId option"
         
fixes invocationCalls :: "InvocationId => CallId set"
         
fixes calls :: "CallId => callInfo"
         
fixes calls3 :: "CallId => callInfo"
         
fixes for_snapshot_additions1 :: "bool"
         
fixes vis :: "CallId set"
         
fixes query_message_author_get_postcondition :: "bool"
         
fixes chat_contains_res3 :: "MessageId => bool"
         
fixes transactionOrigin :: "TxId => InvocationId option"
         
fixes callOrigin1 :: "CallId => TxId option"
         
fixes message_exists_res3 :: "MessageId => bool"
         
fixes vis2 :: "CallId set"
         
fixes calls2 :: "CallId => callInfo"
         
fixes c21 :: "CallId"
         
fixes message_author_get :: "UserId"
         
fixes for_snapshot_additions_with_visibleCalls1 :: "bool"
         
fixes happensBefore1 :: "CallId => CallId set"
         
fixes chat_contains_res1 :: "MessageId => bool"
         
fixes author2 :: "UserId"
         
fixes chat_contains_res2 :: "MessageId => bool"
         
fixes generatedIds_MessageId1 :: "MessageId => InvocationId option"
         
fixes generatedIds_MessageId :: "MessageId => InvocationId option"
         
fixes m_init :: "MessageId"
         
fixes snapshotAddition1 :: "CallId set"
         
fixes currentInvocation :: "InvocationId"
         
fixes message_exists_res2 :: "MessageId => bool"
         
fixes invocationCalls1 :: "InvocationId => CallId set"
         
fixes c0 :: "CallId"
         
fixes knownIds_MessageId :: "MessageId set"
         
fixes query_message_content_getFirst_postcondition :: "bool"
         
fixes message_exists_res :: "MessageId => bool"
         
fixes happensBefore2 :: "CallId => CallId set"
         
fixes m3 :: "MessageId"
         
fixes message_exists_res1 :: "MessageId => bool"
         
fixes message_content_getFirst :: "String"
         
fixes q__query_message_author_get_res_2 :: "UserId"
         
fixes for_snapshot_additions_with_visibleCalls :: "bool"
         
fixes happensBefore :: "CallId => CallId set"
         
       fixes newTxns :: "TxId set"
         
assumes before_procedure_invocation_snapshot_addition_transaction_consistent:

        "(∀bound_c118.
         (∀bound_c215.
           (((bound_c118 ∈ snapshotAddition) ∧ (bound_c215 ∈ (happensBefore bound_c118))) ⟶ (bound_c215 ∈ snapshotAddition))))"
         
assumes before_procedure_invocation_snapshot_addition_transaction_consistent_2:

        "(∀bound_c117.
         (∀bound_c214.
           (((bound_c117 ∈ snapshotAddition) ∧ ((callOrigin bound_c117) = (callOrigin bound_c214)))
             ⟶ (bound_c214 ∈ snapshotAddition))))"
         
assumes before_procedure_invocation_snapshot_addition_subset_calls:

        "(∀bound_c41. ((bound_c41 ∈ snapshotAddition) ⟶ ((calls bound_c41) ≠ no_call)))"
         
assumes before_procedure_invocation_MessageId_knownIds_are_generated:

        "(∀bound_x3. ((bound_x3 ∈ knownIds_MessageId) ⟶ ¬((generatedIds_MessageId bound_x3) = None)))"
         
assumes before_procedure_invocation_message_delete_call_parameter_key_generated:

        "(∀bound_c40.
         (∀bound_key2. (((calls bound_c40) = (message_delete bound_key2)) ⟶ ¬((generatedIds_MessageId bound_key2) = None))))"
         
assumes before_procedure_invocation_message_content_assign_call_parameter_key_generated:

        "(∀bound_c39.
         (∀bound_key1.
           (∀bound_value1.
             (((calls bound_c39) = (message_content_assign bound_key1 bound_value1))
               ⟶ ¬((generatedIds_MessageId bound_key1) = None)))))"
         
assumes before_procedure_invocation_message_author_assign_call_parameter_key_generated:

        "(∀bound_c38.
         (∀bound_key.
           (∀bound_value.
             (((calls bound_c38) = (message_author_assign bound_key bound_value))
               ⟶ ¬((generatedIds_MessageId bound_key) = None)))))"
         
assumes before_procedure_invocation_chat_remove_call_parameter_elem_generated:

        "(∀bound_c37.
         (∀bound_elem1. (((calls bound_c37) = (chat_remove bound_elem1)) ⟶ ¬((generatedIds_MessageId bound_elem1) = None))))"
         
assumes before_procedure_invocation_chat_add_call_parameter_elem_generated:

        "(∀bound_c36.
         (∀bound_elem. (((calls bound_c36) = (chat_add bound_elem)) ⟶ ¬((generatedIds_MessageId bound_elem) = None))))"
         
assumes before_procedure_invocation_sendMessage_result_known:

        "(∀bound_i15.
         (∀bound_result. (((invocationRes bound_i15) = (sendMessage_res bound_result)) ⟶ (bound_result ∈ knownIds_MessageId))))"
         
assumes before_procedure_invocation_getMessage_parameter_m_known:

        "(∀bound_i14. (∀bound_m5. (((invocationOp bound_i14) = (getMessage bound_m5)) ⟶ (bound_m5 ∈ knownIds_MessageId))))"
         
assumes before_procedure_invocation_deleteMessage_parameter_message_id_known:

        "(∀bound_i13.
         (∀bound_message_id.
           (((invocationOp bound_i13) = (deleteMessage bound_message_id)) ⟶ (bound_message_id ∈ knownIds_MessageId))))"
         
assumes before_procedure_invocation_editMessage_parameter_id_known:

        "(∀bound_i12.
         (∀bound_id6.
           (∀bound_newContent2.
             (((invocationOp bound_i12) = (editMessage bound_id6 bound_newContent2)) ⟶ (bound_id6 ∈ knownIds_MessageId)))))"
         
assumes before_procedure_invocation_WF_transactionOrigin_exists:

        "(∀bound_tx9.
         (∀bound_i11. (((transactionOrigin bound_tx9) = (Some bound_i11)) ⟶ ((invocationOp bound_i11) ≠ no_invocation))))"
         
assumes before_procedure_invocation_WF_callOrigin_exists:

        "(∀bound_ca1. (∀bound_tx8. (((callOrigin bound_ca1) = (Some bound_tx8)) ⟶ ¬((transactionOrigin bound_tx8) = None))))"
         
assumes before_procedure_invocation_WF_no_call_implies_not_in_happensBefore:

        "(∀bound_ca. (∀bound_cb. (((callOrigin bound_ca) = None) ⟶ ¬(bound_ca ∈ (happensBefore bound_cb)))))"
         
assumes before_procedure_invocation_WF_no_call_implies_no_happensBefore:

        "(∀bound_c35. (((callOrigin bound_c35) = None) ⟶ ((happensBefore bound_c35) = {})))"
         
assumes before_procedure_invocation_WF_transactionOrigin_callOrigin:

        "(∀bound_tx7. (((transactionOrigin bound_tx7) = None) ⟶ (∀bound_c34. ((callOrigin bound_c34) ≠ (Some bound_tx7)))))"
         
assumes before_procedure_invocation_WF_callOrigin:

        "(∀bound_c33. (((callOrigin bound_c33) = None) = ((calls bound_c33) = no_call)))"
         
assumes before_procedure_invocation_WF_invocationCalls:

        "(∀bound_i10.
         (∀bound_c30.
           ((bound_c30 ∈ (invocationCalls bound_i10))
             = (∃bound_tx6. (((callOrigin bound_c30) = (Some bound_tx6)) ∧ ((transactionOrigin bound_tx6) = (Some bound_i10)))))))"
         
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

        "(∀bound_i9. (((invocationOp bound_i9) = no_invocation) ⟶ ((invocationRes bound_i9) = NoReturn)))"
         
assumes before_procedure_invocation_happensBefore_antisym:

        "(∀bound_x1.
         (∀bound_y1. (((bound_x1 ∈ (happensBefore bound_y1)) ∧ (bound_y1 ∈ (happensBefore bound_x1))) ⟶ (bound_x1 = bound_y1))))"
         
assumes before_procedure_invocation_happensBefore_trans:

        "(∀bound_x.
         (∀bound_y.
           (∀bound_z.
             (((bound_x ∈ (happensBefore bound_y)) ∧ (bound_y ∈ (happensBefore bound_z))) ⟶ (bound_x ∈ (happensBefore bound_y))))))"
         
assumes before_procedure_invocation_happensBefore_reflex:

        "(∀bound_c20. (((calls bound_c20) ≠ no_call) ⟶ (bound_c20 ∈ (happensBefore bound_c20))))"
         
assumes before_procedure_invocation_visibleCalls_causally_consistent:

        "(∀bound_c116. (∀bound_c213. (((bound_c213 ∈ {}) ∧ (bound_c116 ∈ (happensBefore bound_c213))) ⟶ (bound_c116 ∈ {}))))"
         
assumes before_procedure_invocation_visibleCalls_transaction_consistent1:

        "(∀bound_c115.
         (∀bound_c212.
           ((((bound_c115 ∈ {}) ∧ ((callOrigin bound_c115) = (callOrigin bound_c212))) ∧ ((calls bound_c212) ≠ no_call))
             ⟶ (bound_c212 ∈ {}))))"
         
assumes before_procedure_invocation_visibleCalls_exist:

        "(∀bound_c10. ((bound_c10 ∈ {}) ⟶ ((calls bound_c10) ≠ no_call)))"
         
assumes before_procedure_invocation_invocation_sequential:

        "(∀bound_c114.
         (∀bound_tx11.
           (∀bound_i8.
             (∀bound_c211.
               (∀bound_tx24.
                 ((((((callOrigin bound_c114) = (Some bound_tx11)) ∧ ((transactionOrigin bound_tx11) = (Some bound_i8)))
                   ∧ ((callOrigin bound_c211) = (Some bound_tx24)))
                   ∧ ((transactionOrigin bound_tx24) = (Some bound_i8)))
                   ⟶ ((bound_c114 ∈ (happensBefore bound_c211)) ∨ (bound_c211 ∈ (happensBefore bound_c114)))))))))"
         
assumes before_procedure_invocation_happensBefore_exists_r:

        "(∀bound_c113. (∀bound_c210. (((calls bound_c113) = no_call) ⟶ ¬(bound_c113 ∈ (happensBefore bound_c210)))))"
         
assumes before_procedure_invocation_happensBefore_exists_l:

        "(∀bound_c112. (∀bound_c29. (((calls bound_c112) = no_call) ⟶ ¬(bound_c112 ∈ (happensBefore bound_c29)))))"
         
assumes no_call_in_new_invocation:

        "((invocationCalls currentInvocation) = {})"
         
assumes no_transaction_in_new_invocation:

        "(∀tx. ((transactionOrigin tx) ≠ (Some currentInvocation)))"
         
assumes before_procedure_invocation_invariant_10:

        "(∀bound_i7.
         (∀bound_id5.
           (((invocationOp bound_i7) = (getMessage bound_id5))
             ⟶ (((invocationRes bound_i7) = (NoResult )) ∨ (∃bound_r1. ((invocationRes bound_i7) = (getMessage_res bound_r1)))))))"
         
assumes before_procedure_invocation_invariant_9:

        "(∀bound_i6.
         (∀bound_id4.
           (((invocationOp bound_i6) = (deleteMessage bound_id4))
             ⟶ (((invocationRes bound_i6) = (NoResult )) ∨ ((invocationRes bound_i6) = (deleteMessage_res ))))))"
         
assumes before_procedure_invocation_invariant_8:

        "(∀bound_i5.
         (∀bound_id3.
           (∀bound_newContent1.
             (((invocationOp bound_i5) = (editMessage bound_id3 bound_newContent1))
               ⟶ (((invocationRes bound_i5) = (NoResult )) ∨ ((invocationRes bound_i5) = (editMessage_res )))))))"
         
assumes before_procedure_invocation_invariant_7:

        "(∀bound_i4.
         (∀bound_from1.
           (∀bound_content3.
             (((invocationOp bound_i4) = (sendMessage bound_from1 bound_content3))
               ⟶ (((invocationRes bound_i4) = (NoResult )) ∨ (∃bound_r. ((invocationRes bound_i4) = (sendMessage_res bound_r))))))))"
         
assumes before_procedure_invocation_invariant_6:

        "(∀bound_i3.
         (∀bound_id2.
           (((invocationOp bound_i3) = (getMessage bound_id2))
             ⟶ (∀bound_tx5.
               (((transactionOrigin bound_tx5) = (Some bound_i3))
                 ⟶ ((∀bound_tx23. (((transactionOrigin bound_tx23) = (Some bound_i3)) ⟶ (bound_tx5 = bound_tx23)))
                   ∧ ((∃bound_c110.
                     (∃bound_c28.
                       (∃bound_c32.
                         (∃bound_u1.
                           (∃bound_s2.
                             ((((((((((callOrigin bound_c110) = (Some bound_tx5))
                               ∧ ((calls bound_c110) = (queryop_message_exists bound_id2 true)))
                               ∧ (bound_c110 ∈ (happensBefore bound_c28)))
                               ∧ ((callOrigin bound_c28) = (Some bound_tx5)))
                               ∧ ((calls bound_c28) = (queryop_message_author_get bound_id2 bound_u1)))
                               ∧ (bound_c28 ∈ (happensBefore bound_c32)))
                               ∧ ((callOrigin bound_c32) = (Some bound_tx5)))
                               ∧ ((calls bound_c32) = (queryop_message_content_getFirst bound_id2 bound_s2)))
                               ∧ (∀bound_c8.
                                 (((callOrigin bound_c8) = (Some bound_tx5))
                                   ⟶ (((bound_c8 = bound_c110) ∨ (bound_c8 = bound_c28)) ∨ (bound_c8 = bound_c32))))))))))
                     ∨ (∃bound_c111.
                       ((((callOrigin bound_c111) = (Some bound_tx5))
                         ∧ ((calls bound_c111) = (queryop_message_exists bound_id2 false)))
                         ∧ (∀bound_c9. (((callOrigin bound_c9) = (Some bound_tx5)) ⟶ (bound_c9 = bound_c111))))))))))))"
         
assumes before_procedure_invocation_invariant_5:

        "(∀bound_i2.
         (∀bound_id1.
           (((invocationOp bound_i2) = (deleteMessage bound_id1))
             ⟶ (∀bound_tx4.
               (((transactionOrigin bound_tx4) = (Some bound_i2))
                 ⟶ ((∀bound_tx22. (((transactionOrigin bound_tx22) = (Some bound_i2)) ⟶ (bound_tx4 = bound_tx22)))
                   ∧ ((∃bound_c18.
                     (∃bound_c27.
                       (∃bound_c31.
                         ((((((((((callOrigin bound_c18) = (Some bound_tx4))
                           ∧ ((calls bound_c18) = (queryop_message_exists bound_id1 true)))
                           ∧ (bound_c18 ∈ (happensBefore bound_c27)))
                           ∧ ((callOrigin bound_c27) = (Some bound_tx4)))
                           ∧ ((calls bound_c27) = (chat_remove bound_id1)))
                           ∧ (bound_c27 ∈ (happensBefore bound_c31)))
                           ∧ ((callOrigin bound_c31) = (Some bound_tx4)))
                           ∧ ((calls bound_c31) = (message_delete bound_id1)))
                           ∧ (∀bound_c6.
                             (((callOrigin bound_c6) = (Some bound_tx4))
                               ⟶ (((bound_c6 = bound_c18) ∨ (bound_c6 = bound_c27)) ∨ (bound_c6 = bound_c31))))))))
                     ∨ (∃bound_c19.
                       ((((callOrigin bound_c19) = (Some bound_tx4))
                         ∧ ((calls bound_c19) = (queryop_message_exists bound_id1 false)))
                         ∧ (∀bound_c7. (((callOrigin bound_c7) = (Some bound_tx4)) ⟶ (bound_c7 = bound_c19))))))))))))"
         
assumes before_procedure_invocation_invariant_4:

        "(∀bound_i1.
         (∀bound_id.
           (∀bound_newContent.
             (((invocationOp bound_i1) = (editMessage bound_id bound_newContent))
               ⟶ (∀bound_tx3.
                 (((transactionOrigin bound_tx3) = (Some bound_i1))
                   ⟶ ((∀bound_tx21. (((transactionOrigin bound_tx21) = (Some bound_i1)) ⟶ (bound_tx3 = bound_tx21)))
                     ∧ ((∃bound_c16.
                       (∃bound_c26.
                         (((((((callOrigin bound_c16) = (Some bound_tx3))
                           ∧ ((calls bound_c16) = (queryop_message_exists bound_id true)))
                           ∧ (bound_c16 ∈ (happensBefore bound_c26)))
                           ∧ ((callOrigin bound_c26) = (Some bound_tx3)))
                           ∧ ((calls bound_c26) = (message_content_assign bound_id bound_newContent)))
                           ∧ (∀bound_c4.
                             (((callOrigin bound_c4) = (Some bound_tx3)) ⟶ ((bound_c4 = bound_c16) ∨ (bound_c4 = bound_c26)))))))
                       ∨ (∃bound_c17.
                         ((((callOrigin bound_c17) = (Some bound_tx3))
                           ∧ ((calls bound_c17) = (queryop_message_exists bound_id false)))
                           ∧ (∀bound_c5. (((callOrigin bound_c5) = (Some bound_tx3)) ⟶ (bound_c5 = bound_c17)))))))))))))"
         
assumes before_procedure_invocation_invariant_3:

        "(∀bound_i.
         (∀bound_from.
           (∀bound_content1.
             (((invocationOp bound_i) = (sendMessage bound_from bound_content1))
               ⟶ (∀bound_tx1.
                 (((transactionOrigin bound_tx1) = (Some bound_i))
                   ⟶ ((∀bound_tx2. (((transactionOrigin bound_tx2) = (Some bound_i)) ⟶ (bound_tx1 = bound_tx2)))
                     ∧ (∃bound_c15.
                       (∃bound_c25.
                         (∃bound_c3.
                           (∃bound_m4.
                             ((((((((((callOrigin bound_c15) = (Some bound_tx1))
                               ∧ ((calls bound_c15) = (message_author_assign bound_m4 bound_from)))
                               ∧ (bound_c15 ∈ (happensBefore bound_c25)))
                               ∧ ((callOrigin bound_c25) = (Some bound_tx1)))
                               ∧ ((calls bound_c25) = (message_content_assign bound_m4 bound_content1)))
                               ∧ (bound_c25 ∈ (happensBefore bound_c3)))
                               ∧ ((callOrigin bound_c3) = (Some bound_tx1)))
                               ∧ ((calls bound_c3) = (chat_add bound_m4)))
                               ∧ (∀bound_c.
                                 (((callOrigin bound_c) = (Some bound_tx1))
                                   ⟶ (((bound_c = bound_c15) ∨ (bound_c = bound_c25)) ∨ (bound_c = bound_c3))))))))))))))))"
         
assumes before_procedure_invocation_invariant_2:

        "(∀bound_c14.
         (∀bound_m3.
           (∀bound_s1.
             (((calls bound_c14) = (message_content_assign bound_m3 bound_s1))
               ⟶ (∃bound_c24.
                 (∃bound_u.
                   (((calls bound_c24) = (message_author_assign bound_m3 bound_u)) ∧ (bound_c24 ∈ (happensBefore bound_c14)))))))))"
         
assumes before_procedure_invocation_invariant_1:

        "(∀bound_g.
         (∀bound_m2.
           (∀bound_author.
             (∀bound_content.
               ((((invocationOp bound_g) = (getMessage bound_m2))
                 ∧ ((invocationRes bound_g) = (getMessage_res (found bound_author bound_content))))
                 ⟶ (∃bound_s. (∃bound_content2. ((invocationOp bound_s) = (sendMessage bound_author bound_content2)))))))))"
         
assumes chat_contains_res_def:

        "(∀bound_m.
         ((chat_contains_res bound_m)
           = (∃bound_c1.
           (((bound_c1 ∈ snapshotAddition) ∧ ((calls bound_c1) = (chat_add bound_m)))
             ∧ (∀bound_c2.
               (((bound_c2 ∈ snapshotAddition) ∧ ((calls bound_c2) = (chat_remove bound_m)))
                 ⟶ (bound_c2 ∈ (happensBefore bound_c1))))))))"
         
assumes message_exists_res_def:

        "(∀bound_m.
         ((message_exists_res bound_m)
           = (∃bound_c11.
           ((((bound_c11 ∈ snapshotAddition) ∧ (∃bound_args. ((calls bound_c11) = (message_author_assign bound_m bound_args))))
             ∨ ((bound_c11 ∈ snapshotAddition)
               ∧ (∃bound_args1. ((calls bound_c11) = (message_content_assign bound_m bound_args1)))))
             ∧ (∀bound_c21.
               (((bound_c21 ∈ snapshotAddition) ∧ ((calls bound_c21) = (message_delete bound_m)))
                 ⟶ (bound_c21 ∈ (happensBefore bound_c11))))))))"
         
assumes for_snapshot_additions_def:

        "(for_snapshot_additions = (∀bound_m. ((chat_contains_res bound_m) ⟶ (message_exists_res bound_m))))"
         
assumes chat_contains_res_def_2:

        "(∀bound_m1.
         ((chat_contains_res1 bound_m1)
           = (∃bound_c12.
           (((bound_c12 ∈ ({} ∪ snapshotAddition)) ∧ ((calls bound_c12) = (chat_add bound_m1)))
             ∧ (∀bound_c22.
               (((bound_c22 ∈ ({} ∪ snapshotAddition)) ∧ ((calls bound_c22) = (chat_remove bound_m1)))
                 ⟶ (bound_c22 ∈ (happensBefore bound_c12))))))))"
         
assumes message_exists_res_def_2:

        "(∀bound_m1.
         ((message_exists_res1 bound_m1)
           = (∃bound_c13.
           ((((bound_c13 ∈ ({} ∪ snapshotAddition))
             ∧ (∃bound_args2. ((calls bound_c13) = (message_author_assign bound_m1 bound_args2))))
             ∨ ((bound_c13 ∈ ({} ∪ snapshotAddition))
               ∧ (∃bound_args3. ((calls bound_c13) = (message_content_assign bound_m1 bound_args3)))))
             ∧ (∀bound_c23.
               (((bound_c23 ∈ ({} ∪ snapshotAddition)) ∧ ((calls bound_c23) = (message_delete bound_m1)))
                 ⟶ (bound_c23 ∈ (happensBefore bound_c13))))))))"
         
assumes for_snapshot_additions_with_visibleCalls_def:

        "(for_snapshot_additions_with_visibleCalls
         = (∀bound_m1. ((chat_contains_res1 bound_m1) ⟶ (message_exists_res1 bound_m1))))"
         
assumes before_procedure_invocation_invariant_0:

        "(for_snapshot_additions ∧ for_snapshot_additions_with_visibleCalls)"
         
assumes i_fresh:

        "((invocationOp currentInvocation) = no_invocation)"
         
assumes old_transactions_unchanged:

        "(∀c6.
         (∀tx4.
           (((((calls c6) = no_call) ∧ ((calls1 c6) ≠ no_call)) ∧ ((callOrigin1 c6) = (Some tx4)))
             ⟶ ((transactionOrigin tx4) = None))))"
         
assumes growth_invocation_res:

        "(∀i5. (((invocationRes i5) ≠ NoReturn) ⟶ ((invocationRes i5) = (invocationRes i5))))"
         
assumes growth_invocation_op:

        "(∀i4.
         ((((invocationOp(currentInvocation := (getMessage m_init))) i4) ≠ no_invocation)
           ⟶ (((invocationOp(currentInvocation := (getMessage m_init))) i4)
             = ((invocationOp(currentInvocation := (getMessage m_init))) i4))))"
         
assumes growth_tx_origin:

        "(∀tx3. (¬((transactionOrigin tx3) = None) ⟶ ((transactionOrigin1 tx3) = (transactionOrigin tx3))))"
         
assumes growth_call_tx:

        "(∀c5. (((calls c5) ≠ no_call) ⟶ ((callOrigin1 c5) = (callOrigin c5))))"
         
assumes growth_happensbefore:

        "(∀c4. (((calls c4) ≠ no_call) ⟶ ((happensBefore1 c4) = (happensBefore c4))))"
         
assumes growth_calls:

        "(∀c3. (((calls c3) ≠ no_call) ⟶ ((calls1 c3) = (calls c3))))"
         
assumes growth_visible_calls:

        "(∀c2. ((c2 ∈ {}) ⟶ (c2 ∈ {})))"
         
assumes growth_callOrigin:

        "(∀c. (∀tx2. (((callOrigin c) = (Some tx2)) ⟶ ((callOrigin1 c) = (Some tx2)))))"
         
assumes transaction_begin_snapshot_addition_transaction_consistent:

        "(∀bound_c141.
         (∀bound_c236.
           (((bound_c141 ∈ snapshotAddition1) ∧ (bound_c236 ∈ (happensBefore1 bound_c141))) ⟶ (bound_c236 ∈ snapshotAddition1))))"
         
assumes transaction_begin_snapshot_addition_transaction_consistent_2:

        "(∀bound_c140.
         (∀bound_c235.
           (((bound_c140 ∈ snapshotAddition1) ∧ ((callOrigin1 bound_c140) = (callOrigin1 bound_c235)))
             ⟶ (bound_c235 ∈ snapshotAddition1))))"
         
assumes transaction_begin_snapshot_addition_subset_calls:

        "(∀bound_c60. ((bound_c60 ∈ snapshotAddition1) ⟶ ((calls1 bound_c60) ≠ no_call)))"
         
assumes transaction_begin_MessageId_knownIds_are_generated:

        "(∀bound_x6. ((bound_x6 ∈ knownIds_MessageId1) ⟶ ¬((generatedIds_MessageId1 bound_x6) = None)))"
         
assumes transaction_begin_message_delete_call_parameter_key_generated:

        "(∀bound_c59.
         (∀bound_key5. (((calls1 bound_c59) = (message_delete bound_key5)) ⟶ ¬((generatedIds_MessageId1 bound_key5) = None))))"
         
assumes transaction_begin_message_content_assign_call_parameter_key_generated:

        "(∀bound_c58.
         (∀bound_key4.
           (∀bound_value3.
             (((calls1 bound_c58) = (message_content_assign bound_key4 bound_value3))
               ⟶ ¬((generatedIds_MessageId1 bound_key4) = None)))))"
         
assumes transaction_begin_message_author_assign_call_parameter_key_generated:

        "(∀bound_c57.
         (∀bound_key3.
           (∀bound_value2.
             (((calls1 bound_c57) = (message_author_assign bound_key3 bound_value2))
               ⟶ ¬((generatedIds_MessageId1 bound_key3) = None)))))"
         
assumes transaction_begin_chat_remove_call_parameter_elem_generated:

        "(∀bound_c56.
         (∀bound_elem3. (((calls1 bound_c56) = (chat_remove bound_elem3)) ⟶ ¬((generatedIds_MessageId1 bound_elem3) = None))))"
         
assumes transaction_begin_chat_add_call_parameter_elem_generated:

        "(∀bound_c55.
         (∀bound_elem2. (((calls1 bound_c55) = (chat_add bound_elem2)) ⟶ ¬((generatedIds_MessageId1 bound_elem2) = None))))"
         
assumes transaction_begin_sendMessage_result_known:

        "(∀bound_i31.
         (∀bound_result1.
           (((invocationRes bound_i31) = (sendMessage_res bound_result1)) ⟶ (bound_result1 ∈ knownIds_MessageId1))))"
         
assumes transaction_begin_getMessage_parameter_m_known:

        "(∀bound_i30.
         (∀bound_m13.
           ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i30) = (getMessage bound_m13))
             ⟶ (bound_m13 ∈ knownIds_MessageId1))))"
         
assumes transaction_begin_deleteMessage_parameter_message_id_known:

        "(∀bound_i29.
         (∀bound_message_id1.
           ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i29) = (deleteMessage bound_message_id1))
             ⟶ (bound_message_id1 ∈ knownIds_MessageId1))))"
         
assumes transaction_begin_editMessage_parameter_id_known:

        "(∀bound_i28.
         (∀bound_id13.
           (∀bound_newContent5.
             ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i28)
               = (editMessage bound_id13 bound_newContent5))
               ⟶ (bound_id13 ∈ knownIds_MessageId1)))))"
         
assumes transaction_begin_WF_transactionOrigin_exists:

        "(∀bound_tx19.
         (∀bound_i27.
           (((transactionOrigin1 bound_tx19) = (Some bound_i27))
             ⟶ (((invocationOp(currentInvocation := (getMessage m_init))) bound_i27) ≠ no_invocation))))"
         
assumes transaction_begin_WF_callOrigin_exists:

        "(∀bound_ca3.
         (∀bound_tx18. (((callOrigin1 bound_ca3) = (Some bound_tx18)) ⟶ ¬((transactionOrigin1 bound_tx18) = None))))"
         
assumes transaction_begin_WF_no_call_implies_not_in_happensBefore:

        "(∀bound_ca2. (∀bound_cb1. (((callOrigin1 bound_ca2) = None) ⟶ ¬(bound_ca2 ∈ (happensBefore1 bound_cb1)))))"
         
assumes transaction_begin_WF_no_call_implies_no_happensBefore:

        "(∀bound_c54. (((callOrigin1 bound_c54) = None) ⟶ ((happensBefore1 bound_c54) = {})))"
         
assumes transaction_begin_WF_transactionOrigin_callOrigin:

        "(∀bound_tx17. (((transactionOrigin1 bound_tx17) = None) ⟶ (∀bound_c53. ((callOrigin1 bound_c53) ≠ (Some bound_tx17)))))"
         
assumes transaction_begin_WF_callOrigin:

        "(∀bound_c52. (((callOrigin1 bound_c52) = None) = ((calls1 bound_c52) = no_call)))"
         
assumes transaction_begin_WF_invocationCalls:

        "(∀bound_i26.
         (∀bound_c51.
           ((bound_c51 ∈ (invocationCalls1 bound_i26))
             = (∃bound_tx16.
             (((callOrigin1 bound_c51) = (Some bound_tx16)) ∧ ((transactionOrigin1 bound_tx16) = (Some bound_i26)))))))"
         
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

        "(∀bound_i25.
         ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i25) = no_invocation)
           ⟶ ((invocationRes bound_i25) = NoReturn)))"
         
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

        "(∀bound_c50. (((calls1 bound_c50) ≠ no_call) ⟶ (bound_c50 ∈ (happensBefore1 bound_c50))))"
         
assumes transaction_begin_visibleCalls_causally_consistent:

        "(∀bound_c139. (∀bound_c234. (((bound_c234 ∈ vis) ∧ (bound_c139 ∈ (happensBefore1 bound_c234))) ⟶ (bound_c139 ∈ vis))))"
         
assumes transaction_begin_visibleCalls_transaction_consistent1:

        "(∀bound_c138.
         (∀bound_c233.
           ((((bound_c138 ∈ vis) ∧ ((callOrigin1 bound_c138) = (callOrigin1 bound_c233))) ∧ ((calls1 bound_c233) ≠ no_call))
             ⟶ (bound_c233 ∈ vis))))"
         
assumes transaction_begin_visibleCalls_exist:

        "(∀bound_c49. ((bound_c49 ∈ vis) ⟶ ((calls1 bound_c49) ≠ no_call)))"
         
assumes transaction_begin_invocation_sequential:

        "(∀bound_c137.
         (∀bound_tx15.
           (∀bound_i24.
             (∀bound_c232.
               (∀bound_tx29.
                 ((((((callOrigin1 bound_c137) = (Some bound_tx15)) ∧ ((transactionOrigin1 bound_tx15) = (Some bound_i24)))
                   ∧ ((callOrigin1 bound_c232) = (Some bound_tx29)))
                   ∧ ((transactionOrigin1 bound_tx29) = (Some bound_i24)))
                   ⟶ ((bound_c137 ∈ (happensBefore1 bound_c232)) ∨ (bound_c232 ∈ (happensBefore1 bound_c137)))))))))"
         
assumes transaction_begin_happensBefore_exists_r:

        "(∀bound_c136. (∀bound_c231. (((calls1 bound_c136) = no_call) ⟶ ¬(bound_c136 ∈ (happensBefore1 bound_c231)))))"
         
assumes transaction_begin_happensBefore_exists_l:

        "(∀bound_c135. (∀bound_c230. (((calls1 bound_c135) = no_call) ⟶ ¬(bound_c135 ∈ (happensBefore1 bound_c230)))))"
         
assumes at_transaction_begin_invariant_10:

        "(∀bound_i23.
         (∀bound_id12.
           ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i23) = (getMessage bound_id12))
             ⟶ (((invocationRes bound_i23) = (NoResult ))
               ∨ (∃bound_r5. ((invocationRes bound_i23) = (getMessage_res bound_r5)))))))"
         
assumes at_transaction_begin_invariant_9:

        "(∀bound_i22.
         (∀bound_id11.
           ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i22) = (deleteMessage bound_id11))
             ⟶ (((invocationRes bound_i22) = (NoResult )) ∨ ((invocationRes bound_i22) = (deleteMessage_res ))))))"
         
assumes at_transaction_begin_invariant_8:

        "(∀bound_i21.
         (∀bound_id10.
           (∀bound_newContent4.
             ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i21)
               = (editMessage bound_id10 bound_newContent4))
               ⟶ (((invocationRes bound_i21) = (NoResult )) ∨ ((invocationRes bound_i21) = (editMessage_res )))))))"
         
assumes at_transaction_begin_invariant_7:

        "(∀bound_i20.
         (∀bound_from3.
           (∀bound_content6.
             ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i20) = (sendMessage bound_from3 bound_content6))
               ⟶ (((invocationRes bound_i20) = (NoResult ))
                 ∨ (∃bound_r4. ((invocationRes bound_i20) = (sendMessage_res bound_r4))))))))"
         
assumes at_transaction_begin_invariant_6:

        "(∀bound_i19.
         (∀bound_id9.
           ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i19) = (getMessage bound_id9))
             ⟶ (∀bound_tx14.
               (((transactionOrigin1 bound_tx14) = (Some bound_i19))
                 ⟶ ((∀bound_tx28. (((transactionOrigin1 bound_tx28) = (Some bound_i19)) ⟶ (bound_tx14 = bound_tx28)))
                   ∧ ((∃bound_c133.
                     (∃bound_c229.
                       (∃bound_c312.
                         (∃bound_u4.
                           (∃bound_s6.
                             ((((((((((callOrigin1 bound_c133) = (Some bound_tx14))
                               ∧ ((calls1 bound_c133) = (queryop_message_exists bound_id9 true)))
                               ∧ (bound_c133 ∈ (happensBefore1 bound_c229)))
                               ∧ ((callOrigin1 bound_c229) = (Some bound_tx14)))
                               ∧ ((calls1 bound_c229) = (queryop_message_author_get bound_id9 bound_u4)))
                               ∧ (bound_c229 ∈ (happensBefore1 bound_c312)))
                               ∧ ((callOrigin1 bound_c312) = (Some bound_tx14)))
                               ∧ ((calls1 bound_c312) = (queryop_message_content_getFirst bound_id9 bound_s6)))
                               ∧ (∀bound_c47.
                                 (((callOrigin1 bound_c47) = (Some bound_tx14))
                                   ⟶ (((bound_c47 = bound_c133) ∨ (bound_c47 = bound_c229)) ∨ (bound_c47 = bound_c312))))))))))
                     ∨ (∃bound_c134.
                       ((((callOrigin1 bound_c134) = (Some bound_tx14))
                         ∧ ((calls1 bound_c134) = (queryop_message_exists bound_id9 false)))
                         ∧ (∀bound_c48. (((callOrigin1 bound_c48) = (Some bound_tx14)) ⟶ (bound_c48 = bound_c134))))))))))))"
         
assumes at_transaction_begin_invariant_5:

        "(∀bound_i18.
         (∀bound_id8.
           ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i18) = (deleteMessage bound_id8))
             ⟶ (∀bound_tx13.
               (((transactionOrigin1 bound_tx13) = (Some bound_i18))
                 ⟶ ((∀bound_tx27. (((transactionOrigin1 bound_tx27) = (Some bound_i18)) ⟶ (bound_tx13 = bound_tx27)))
                   ∧ ((∃bound_c131.
                     (∃bound_c228.
                       (∃bound_c311.
                         ((((((((((callOrigin1 bound_c131) = (Some bound_tx13))
                           ∧ ((calls1 bound_c131) = (queryop_message_exists bound_id8 true)))
                           ∧ (bound_c131 ∈ (happensBefore1 bound_c228)))
                           ∧ ((callOrigin1 bound_c228) = (Some bound_tx13)))
                           ∧ ((calls1 bound_c228) = (chat_remove bound_id8)))
                           ∧ (bound_c228 ∈ (happensBefore1 bound_c311)))
                           ∧ ((callOrigin1 bound_c311) = (Some bound_tx13)))
                           ∧ ((calls1 bound_c311) = (message_delete bound_id8)))
                           ∧ (∀bound_c45.
                             (((callOrigin1 bound_c45) = (Some bound_tx13))
                               ⟶ (((bound_c45 = bound_c131) ∨ (bound_c45 = bound_c228)) ∨ (bound_c45 = bound_c311))))))))
                     ∨ (∃bound_c132.
                       ((((callOrigin1 bound_c132) = (Some bound_tx13))
                         ∧ ((calls1 bound_c132) = (queryop_message_exists bound_id8 false)))
                         ∧ (∀bound_c46. (((callOrigin1 bound_c46) = (Some bound_tx13)) ⟶ (bound_c46 = bound_c132))))))))))))"
         
assumes at_transaction_begin_invariant_4:

        "(∀bound_i17.
         (∀bound_id7.
           (∀bound_newContent3.
             ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i17) = (editMessage bound_id7 bound_newContent3))
               ⟶ (∀bound_tx12.
                 (((transactionOrigin1 bound_tx12) = (Some bound_i17))
                   ⟶ ((∀bound_tx26. (((transactionOrigin1 bound_tx26) = (Some bound_i17)) ⟶ (bound_tx12 = bound_tx26)))
                     ∧ ((∃bound_c129.
                       (∃bound_c227.
                         (((((((callOrigin1 bound_c129) = (Some bound_tx12))
                           ∧ ((calls1 bound_c129) = (queryop_message_exists bound_id7 true)))
                           ∧ (bound_c129 ∈ (happensBefore1 bound_c227)))
                           ∧ ((callOrigin1 bound_c227) = (Some bound_tx12)))
                           ∧ ((calls1 bound_c227) = (message_content_assign bound_id7 bound_newContent3)))
                           ∧ (∀bound_c43.
                             (((callOrigin1 bound_c43) = (Some bound_tx12))
                               ⟶ ((bound_c43 = bound_c129) ∨ (bound_c43 = bound_c227)))))))
                       ∨ (∃bound_c130.
                         ((((callOrigin1 bound_c130) = (Some bound_tx12))
                           ∧ ((calls1 bound_c130) = (queryop_message_exists bound_id7 false)))
                           ∧ (∀bound_c44. (((callOrigin1 bound_c44) = (Some bound_tx12)) ⟶ (bound_c44 = bound_c130)))))))))))))"
         
assumes at_transaction_begin_invariant_3:

        "(∀bound_i16.
         (∀bound_from2.
           (∀bound_content5.
             ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i16) = (sendMessage bound_from2 bound_content5))
               ⟶ (∀bound_tx10.
                 (((transactionOrigin1 bound_tx10) = (Some bound_i16))
                   ⟶ ((∀bound_tx25. (((transactionOrigin1 bound_tx25) = (Some bound_i16)) ⟶ (bound_tx10 = bound_tx25)))
                     ∧ (∃bound_c128.
                       (∃bound_c226.
                         (∃bound_c310.
                           (∃bound_m12.
                             ((((((((((callOrigin1 bound_c128) = (Some bound_tx10))
                               ∧ ((calls1 bound_c128) = (message_author_assign bound_m12 bound_from2)))
                               ∧ (bound_c128 ∈ (happensBefore1 bound_c226)))
                               ∧ ((callOrigin1 bound_c226) = (Some bound_tx10)))
                               ∧ ((calls1 bound_c226) = (message_content_assign bound_m12 bound_content5)))
                               ∧ (bound_c226 ∈ (happensBefore1 bound_c310)))
                               ∧ ((callOrigin1 bound_c310) = (Some bound_tx10)))
                               ∧ ((calls1 bound_c310) = (chat_add bound_m12)))
                               ∧ (∀bound_c42.
                                 (((callOrigin1 bound_c42) = (Some bound_tx10))
                                   ⟶ (((bound_c42 = bound_c128) ∨ (bound_c42 = bound_c226)) ∨ (bound_c42 = bound_c310))))))))))))))))"
         
assumes at_transaction_begin_invariant_2:

        "(∀bound_c127.
         (∀bound_m11.
           (∀bound_s5.
             (((calls1 bound_c127) = (message_content_assign bound_m11 bound_s5))
               ⟶ (∃bound_c225.
                 (∃bound_u3.
                   (((calls1 bound_c225) = (message_author_assign bound_m11 bound_u3))
                     ∧ (bound_c225 ∈ (happensBefore1 bound_c127)))))))))"
         
assumes at_transaction_begin_invariant_1:

        "(∀bound_g1.
         (∀bound_m10.
           (∀bound_author1.
             (∀bound_content4.
               (((((invocationOp(currentInvocation := (getMessage m_init))) bound_g1) = (getMessage bound_m10))
                 ∧ ((invocationRes bound_g1) = (getMessage_res (found bound_author1 bound_content4))))
                 ⟶ (∃bound_s4.
                   (∃bound_content22.
                     (((invocationOp(currentInvocation := (getMessage m_init))) bound_s4)
                       = (sendMessage bound_author1 bound_content22)))))))))"
         
assumes chat_contains_res_def_3:

        "(∀bound_m8.
         ((chat_contains_res2 bound_m8)
           = (∃bound_c123.
           (((bound_c123 ∈ snapshotAddition1) ∧ ((calls1 bound_c123) = (chat_add bound_m8)))
             ∧ (∀bound_c221.
               (((bound_c221 ∈ snapshotAddition1) ∧ ((calls1 bound_c221) = (chat_remove bound_m8)))
                 ⟶ (bound_c221 ∈ (happensBefore1 bound_c123))))))))"
         
assumes message_exists_res_def_3:

        "(∀bound_m8.
         ((message_exists_res2 bound_m8)
           = (∃bound_c124.
           ((((bound_c124 ∈ snapshotAddition1)
             ∧ (∃bound_args8. ((calls1 bound_c124) = (message_author_assign bound_m8 bound_args8))))
             ∨ ((bound_c124 ∈ snapshotAddition1)
               ∧ (∃bound_args9. ((calls1 bound_c124) = (message_content_assign bound_m8 bound_args9)))))
             ∧ (∀bound_c222.
               (((bound_c222 ∈ snapshotAddition1) ∧ ((calls1 bound_c222) = (message_delete bound_m8)))
                 ⟶ (bound_c222 ∈ (happensBefore1 bound_c124))))))))"
         
assumes for_snapshot_additions_def_2:

        "(for_snapshot_additions1 = (∀bound_m8. ((chat_contains_res2 bound_m8) ⟶ (message_exists_res2 bound_m8))))"
         
assumes chat_contains_res_def_4:

        "(∀bound_m9.
         ((chat_contains_res3 bound_m9)
           = (∃bound_c125.
           (((bound_c125 ∈ (vis ∪ snapshotAddition1)) ∧ ((calls1 bound_c125) = (chat_add bound_m9)))
             ∧ (∀bound_c223.
               (((bound_c223 ∈ (vis ∪ snapshotAddition1)) ∧ ((calls1 bound_c223) = (chat_remove bound_m9)))
                 ⟶ (bound_c223 ∈ (happensBefore1 bound_c125))))))))"
         
assumes message_exists_res_def_4:

        "(∀bound_m9.
         ((message_exists_res3 bound_m9)
           = (∃bound_c126.
           ((((bound_c126 ∈ (vis ∪ snapshotAddition1))
             ∧ (∃bound_args10. ((calls1 bound_c126) = (message_author_assign bound_m9 bound_args10))))
             ∨ ((bound_c126 ∈ (vis ∪ snapshotAddition1))
               ∧ (∃bound_args11. ((calls1 bound_c126) = (message_content_assign bound_m9 bound_args11)))))
             ∧ (∀bound_c224.
               (((bound_c224 ∈ (vis ∪ snapshotAddition1)) ∧ ((calls1 bound_c224) = (message_delete bound_m9)))
                 ⟶ (bound_c224 ∈ (happensBefore1 bound_c126))))))))"
         
assumes for_snapshot_additions_with_visibleCalls_def_2:

        "(for_snapshot_additions_with_visibleCalls1
         = (∀bound_m9. ((chat_contains_res3 bound_m9) ⟶ (message_exists_res3 bound_m9))))"
         
assumes at_transaction_begin_invariant_0:

        "(for_snapshot_additions1 ∧ for_snapshot_additions_with_visibleCalls1)"
         
assumes no_new_calls_addded_to_current:

        "((invocationCalls currentInvocation) = (invocationCalls1 currentInvocation))"
         
assumes no_new_transactions_added_to_current:

        "(∀bound_t.
         (((transactionOrigin bound_t) = (Some currentInvocation)) = ((transactionOrigin1 bound_t) = (Some currentInvocation))))"
         
assumes tx1_fresh:

        "((transactionOrigin1 tx1) = None)"
         
assumes vis_update:

        "(vis = ({} ∪ newCalls))"
         
assumes new_transactions_exist:

        "(newTxns ⊆ (dom transactionOrigin1))"
         
assumes message_exists_res_def_5:

        "(message_exists_res4
         = (∃bound_c142.
         ((((bound_c142 ∈ vis) ∧ (∃bound_args12. ((calls1 bound_c142) = (message_author_assign m_init bound_args12))))
           ∨ ((bound_c142 ∈ vis) ∧ (∃bound_args13. ((calls1 bound_c142) = (message_content_assign m_init bound_args13)))))
           ∧ (∀bound_c237.
             (((bound_c237 ∈ vis) ∧ ((calls1 bound_c237) = (message_delete m_init)))
               ⟶ (bound_c237 ∈ (happensBefore1 bound_c142)))))))"
         
assumes q__query_message_exists_res_1_assignment:

        "(q__query_message_exists_res_1 = message_exists_res4)"
         
assumes c0_freshB:

        "((calls1 c0) = no_call)"
         
assumes c0_freshA:

        "distinct [c0]"
         
assumes if_statement_condition_true:

        "q__query_message_exists_res_1"
         
assumes q__query_message_author_get_res_2_assignment:

        "(q__query_message_author_get_res_2 = message_author_get)"
         
assumes vis_def:

        "(vis1 = (vis ∪ {c0}))"
         
assumes calls_def:

        "(calls2 = (calls1(c0 := (queryop_message_exists m_init q__query_message_exists_res_1))))"
         
assumes happensBefore_def:

        "(happensBefore2 = (happensBefore1(c0 := (vis ∪ {c0}))))"
         
assumes query_message_author_get_postcondition_def:

        "(query_message_author_get_postcondition
         = (¬(∃bound_c313.
         (∃bound_value21.
           ((bound_c313 ∈ vis1)
             ∧ (((calls2 bound_c313) = (message_author_assign m_init bound_value21))
               ∧ (∀bound_d.
                 (((bound_d ∈ vis1) ∧ ((calls2 bound_d) = (message_delete m_init))) ⟶ (bound_d ∈ (happensBefore2 bound_c313))))))))
         ∨ (∃bound_c143.
           (((bound_c143 ∈ vis1)
             ∧ (((calls2 bound_c143) = (message_author_assign m_init message_author_get))
               ∧ (∀bound_d1.
                 (((bound_d1 ∈ vis1) ∧ ((calls2 bound_d1) = (message_delete m_init)))
                   ⟶ (bound_d1 ∈ (happensBefore2 bound_c143))))))
             ∧ ¬(∃bound_c238.
               (∃bound_value4.
                 ((((bound_c238 ∈ vis1) ∧ (bound_c143 ≠ bound_c238))
                   ∧ (((calls2 bound_c238) = (message_author_assign m_init bound_value4))
                     ∧ (∀bound_d2.
                       (((bound_d2 ∈ vis1) ∧ ((calls2 bound_d2) = (message_delete m_init)))
                         ⟶ (bound_d2 ∈ (happensBefore2 bound_c238))))))
                   ∧ (bound_c143 ∈ (happensBefore2 bound_c238)))))))))"
         
assumes choose_message_author_get:

        "query_message_author_get_postcondition"
         
assumes c11_freshB:

        "((calls2 c11) = no_call)"
         
assumes c11_freshA:

        "distinct [c11 , c0]"
         
assumes q__query_message_content_getFirst_res_3_assignment:

        "(q__query_message_content_getFirst_res_3 = message_content_getFirst)"
         
assumes vis_def_2:

        "(vis2 = (vis1 ∪ {c11}))"
         
assumes calls_def_2:

        "(calls3 = (calls2(c11 := (queryop_message_author_get m_init q__query_message_author_get_res_2))))"
         
assumes happensBefore_def_2:

        "(happensBefore3 = (happensBefore2(c11 := (vis1 ∪ {c11}))))"
         
assumes query_message_content_getFirst_postcondition_def:

        "(query_message_content_getFirst_postcondition
         = (∃bound_c144.
         (((bound_c144 ∈ vis2)
           ∧ (((calls3 bound_c144) = (message_content_assign m_init message_content_getFirst))
             ∧ (∀bound_d3.
               (((bound_d3 ∈ vis2) ∧ ((calls3 bound_d3) = (message_delete m_init))) ⟶ (bound_d3 ∈ (happensBefore3 bound_c144))))))
           ∧ ¬(∃bound_c239.
             ((((bound_c239 ∈ vis2) ∧ (bound_c144 ≠ bound_c239))
               ∧ (((calls3 bound_c239) = (message_content_assign m_init message_content_getFirst))
                 ∧ (∀bound_d4.
                   (((bound_d4 ∈ vis2) ∧ ((calls3 bound_d4) = (message_delete m_init)))
                     ⟶ (bound_d4 ∈ (happensBefore3 bound_c239))))))
               ∧ (bound_c144 ∈ (happensBefore3 bound_c239)))))))"
         
assumes choose_message_content_getFirst:

        "query_message_content_getFirst_postcondition"
         
assumes c21_freshB:

        "((calls3 c21) = no_call)"
         
assumes c21_freshA:

        "distinct [c21 , c0 , c11]"
         
assumes invariant_not_violated:

        "¬(((((invocationOp(currentInvocation := (getMessage m_init))) g1) = (getMessage m3))
         ∧ (((invocationRes(currentInvocation := (getMessage_res (found q__query_message_author_get_res_2
             q__query_message_content_getFirst_res_3)))) g1)
           = (getMessage_res (found author2 content4))))
         ⟶ (∃bound_s7.
           (∃bound_content23.
             (((invocationOp(currentInvocation := (getMessage m_init))) bound_s7) = (sendMessage author2 bound_content23)))))"
         shows False
  using invariant_not_violated proof (rule notE, auto split: if_splits)
  assume a1: "g1 ≠ currentInvocation"
    and a2: "invocationOp g1 = getMessage m3"
    and a3: "invocationRes g1 = getMessage_res (found author2 content4)"
    and a4: "∀bound_s7. bound_s7 = currentInvocation ∨ (∀bound_content23. invocationOp bound_s7 ≠ sendMessage author2 bound_content23)"

  show False
    using a3 at_transaction_begin_invariant_1 invariant_not_violated by blast
next

  assume a1: "g1 = currentInvocation"
and a2: "m_init = m3"
and a3: "q__query_message_author_get_res_2 = author2"
and a4: "q__query_message_content_getFirst_res_3 = content4"
and a5: "∀bound_s7. bound_s7 = currentInvocation ∨ (∀bound_content23. invocationOp bound_s7 ≠ sendMessage author2 bound_content23)"


  from if_statement_condition_true
    q__query_message_exists_res_1_assignment
    message_exists_res_def_5
  obtain c_assign1
    where c_assign1a: "c_assign1 ∈ vis"
      and c_assign1b: "(∃x. calls1 c_assign1 = message_author_assign m_init x) 
         ∨ (∃x. calls1 c_assign1 = message_content_assign m_init x)"
      and c_assign1c: "(∀c. c ∈ vis ∧ calls1 c = message_delete m_init ⟶ c ∈ happensBefore1 c_assign1)"
    by auto



  obtain c_author_assign 
    where "c_author_assign ∈ vis"
      and "(∃x. calls1 c_author_assign = message_author_assign m_init x)"
      and "(∀c. c ∈ vis ∧ calls1 c = message_delete m_init ⟶ c ∈ happensBefore1 c_author_assign)"
    using c_assign1b proof (rule disjE; atomize_elim; auto)
    fix x
    assume a: "calls1 c_assign1 = message_author_assign m_init x"
    thus "∃c_author_assign.
            c_author_assign ∈ vis ∧
            (∃x. calls1 c_author_assign = message_author_assign m_init x) ∧
            (∀c. c ∈ vis ∧ calls1 c = message_delete m_init ⟶ c ∈ happensBefore1 c_author_assign)"
      using c_assign1a c_assign1c by blast
  next
    fix x
    assume a: "calls1 c_assign1 = message_content_assign m_init x"

    find_theorems message_content_assign message_author_assign name: inv

    from at_transaction_begin_invariant_2
    obtain c_assign2 y
      where "calls1 c_assign2 = message_author_assign m_init y"
        and "c_assign2 ∈ happensBefore1 c_assign2"
      apply auto
      using a transaction_begin_happensBefore_exists_r transaction_begin_happensBefore_reflex by blast


    show "∃c_author_assign.
            c_author_assign ∈ vis ∧
            (∃x. calls1 c_author_assign = message_author_assign m_init x) ∧
            (∀c. c ∈ vis ∧ calls1 c = message_delete m_init ⟶ c ∈ happensBefore1 c_author_assign)"
    proof (rule_tac x=c_assign2 in exI, auto)
      show "c_assign2 ∈ vis"
(*from happens before and causal consistency *)
        sorry
      show "∃x. calls1 c_assign2 = message_author_assign m_init x"
        by (simp add: ‹calls1 c_assign2 = message_author_assign m_init y›)

  show "⋀c. ⟦c ∈ vis; calls1 c = message_delete m_init⟧ ⟹ c ∈ happensBefore1 c_assign2"
    sorry

  find_theorems message_exists_res4

  have q__query_message_exists_res_1
    by (simp add: if_statement_condition_true)


  show False

    sorry

