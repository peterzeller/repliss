
theory "getMessage_line33"
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

fixes bound_tx7 :: "TxId"

fixes newCalls :: "CallId set"

fixes g1 :: "InvocationId"

fixes snapshotAddition :: "CallId set"

fixes generatedIds_MessageId1 :: "MessageId => InvocationId option"

fixes m6 :: "MessageId"

fixes message_content_getFirst :: "String"

fixes q__query_message_content_getFirst_res_3 :: "String"

fixes happensBefore3 :: "CallId => CallId set"

fixes knownIds_MessageId1 :: "MessageId set"

fixes transactionOrigin1 :: "TxId => InvocationId option"

fixes bound_tx8 :: "TxId"

fixes bound_tx29 :: "TxId"

fixes c11 :: "CallId"

fixes tx1 :: "TxId"

fixes invocationRes :: "InvocationId => invocationResult"

fixes calls1 :: "CallId => callInfo"

fixes content4 :: "String"

fixes invocationOp :: "InvocationId => invocationInfo"

fixes callOrigin :: "CallId => TxId option"

fixes invocationCalls :: "InvocationId => CallId set"

fixes calls :: "CallId => callInfo"

fixes calls3 :: "CallId => callInfo"

fixes vis :: "CallId set"

fixes query_message_author_get_postcondition :: "bool"

fixes vis2 :: "CallId set"

fixes transactionOrigin :: "TxId => InvocationId option"

fixes callOrigin1 :: "CallId => TxId option"

fixes calls2 :: "CallId => callInfo"

fixes c21 :: "CallId"

fixes bound_tx6 :: "TxId"

fixes message_author_get :: "UserId"

fixes happensBefore1 :: "CallId => CallId set"

fixes author2 :: "UserId"

fixes generatedIds_MessageId :: "MessageId => InvocationId option"

fixes m_init :: "MessageId"

fixes bound_tx31 :: "TxId"

fixes q__query_message_exists_res_1 :: "bool"

fixes snapshotAddition1 :: "CallId set"

fixes currentInvocation :: "InvocationId"

fixes invocationCalls1 :: "InvocationId => CallId set"

fixes c0 :: "CallId"

fixes knownIds_MessageId :: "MessageId set"

fixes query_message_content_getFirst_postcondition :: "bool"

fixes message_exists_res :: "bool"

fixes happensBefore2 :: "CallId => CallId set"

fixes q__query_message_author_get_res_2 :: "UserId"

fixes bound_tx30 :: "TxId"

fixes happensBefore :: "CallId => CallId set"

fixes newTxns :: "TxId set"

assumes before_procedure_invocation_snapshot_addition_transaction_consistent:

"(\<forall>bound_c116.
         (\<forall>bound_c211.
           (((bound_c116 \<in> snapshotAddition) \<and> (bound_c211 \<in> (happensBefore bound_c116))) \<longrightarrow> (bound_c211 \<in> snapshotAddition))))"

assumes before_procedure_invocation_snapshot_addition_transaction_consistent_2:

"(\<forall>bound_c115.
         (\<forall>bound_c210.
           (((bound_c115 \<in> snapshotAddition) \<and> ((callOrigin bound_c115) = (callOrigin bound_c210)))
             \<longrightarrow> (bound_c210 \<in> snapshotAddition))))"

assumes before_procedure_invocation_snapshot_addition_subset_calls:

"(\<forall>bound_c42. ((bound_c42 \<in> snapshotAddition) \<longrightarrow> ((calls bound_c42) \<noteq> no_call)))"

assumes before_procedure_invocation_MessageId_knownIds_are_generated:

"(\<forall>bound_x6. ((bound_x6 \<in> knownIds_MessageId) \<longrightarrow> \<not>((generatedIds_MessageId bound_x6) = None)))"

assumes before_procedure_invocation_message_delete_call_parameter_key_generated:

"(\<forall>bound_c41.
         (\<forall>bound_key2. (((calls bound_c41) = (message_delete bound_key2)) \<longrightarrow> \<not>((generatedIds_MessageId bound_key2) = None))))"

assumes before_procedure_invocation_message_content_assign_call_parameter_key_generated:

"(\<forall>bound_c40.
         (\<forall>bound_key1.
           (\<forall>bound_value1.
             (((calls bound_c40) = (message_content_assign bound_key1 bound_value1))
               \<longrightarrow> \<not>((generatedIds_MessageId bound_key1) = None)))))"

assumes before_procedure_invocation_message_author_assign_call_parameter_key_generated:

"(\<forall>bound_c39.
         (\<forall>bound_key.
           (\<forall>bound_value.
             (((calls bound_c39) = (message_author_assign bound_key bound_value))
               \<longrightarrow> \<not>((generatedIds_MessageId bound_key) = None)))))"

assumes before_procedure_invocation_chat_remove_call_parameter_elem_generated:

"(\<forall>bound_c38.
         (\<forall>bound_elem1. (((calls bound_c38) = (chat_remove bound_elem1)) \<longrightarrow> \<not>((generatedIds_MessageId bound_elem1) = None))))"

assumes before_procedure_invocation_chat_add_call_parameter_elem_generated:

"(\<forall>bound_c37.
         (\<forall>bound_elem. (((calls bound_c37) = (chat_add bound_elem)) \<longrightarrow> \<not>((generatedIds_MessageId bound_elem) = None))))"

assumes before_procedure_invocation_sendMessage_result_known:

"(\<forall>bound_i18.
         (\<forall>bound_result. (((invocationRes bound_i18) = (sendMessage_res bound_result)) \<longrightarrow> (bound_result \<in> knownIds_MessageId))))"

assumes before_procedure_invocation_getMessage_parameter_m_known:

"(\<forall>bound_i17. (\<forall>bound_m7. (((invocationOp bound_i17) = (getMessage bound_m7)) \<longrightarrow> (bound_m7 \<in> knownIds_MessageId))))"

assumes before_procedure_invocation_deleteMessage_parameter_message_id_known:

"(\<forall>bound_i16.
         (\<forall>bound_message_id.
           (((invocationOp bound_i16) = (deleteMessage bound_message_id)) \<longrightarrow> (bound_message_id \<in> knownIds_MessageId))))"

assumes before_procedure_invocation_editMessage_parameter_id_known:

"(\<forall>bound_i15.
         (\<forall>bound_id6.
           (\<forall>bound_newContent2.
             (((invocationOp bound_i15) = (editMessage bound_id6 bound_newContent2)) \<longrightarrow> (bound_id6 \<in> knownIds_MessageId)))))"

assumes before_procedure_invocation_WF_transactionOrigin_exists:

"(\<forall>bound_tx13.
         (\<forall>bound_i14. (((transactionOrigin bound_tx13) = (Some bound_i14)) \<longrightarrow> ((invocationOp bound_i14) \<noteq> no_invocation))))"

assumes before_procedure_invocation_WF_callOrigin_exists:

"(\<forall>bound_ca1. (\<forall>bound_tx12. (((callOrigin bound_ca1) = (Some bound_tx12)) \<longrightarrow> \<not>((transactionOrigin bound_tx12) = None))))"

assumes before_procedure_invocation_WF_no_call_implies_not_in_happensBefore:

"(\<forall>bound_ca. (\<forall>bound_cb. (((callOrigin bound_ca) = None) \<longrightarrow> \<not>(bound_ca \<in> (happensBefore bound_cb)))))"

assumes before_procedure_invocation_WF_no_call_implies_no_happensBefore:

"(\<forall>bound_c36. (((callOrigin bound_c36) = None) \<longrightarrow> ((happensBefore bound_c36) = {})))"

assumes before_procedure_invocation_WF_transactionOrigin_callOrigin:

"(\<forall>bound_tx10. (((transactionOrigin bound_tx10) = None) \<longrightarrow> (\<forall>bound_c35. ((callOrigin bound_c35) \<noteq> (Some bound_tx10)))))"

assumes before_procedure_invocation_WF_callOrigin:

"(\<forall>bound_c34. (((callOrigin bound_c34) = None) = ((calls bound_c34) = no_call)))"

assumes before_procedure_invocation_WF_invocationCalls:

"(\<forall>bound_i13.
         (\<forall>bound_c33.
           ((bound_c33 \<in> (invocationCalls bound_i13))
             = (\<exists>bound_tx9. (((callOrigin bound_c33) = (Some bound_tx9)) \<and> ((transactionOrigin bound_tx9) = (Some bound_i13)))))))"

assumes before_procedure_invocation_no_invocation_implies_no_result:

"(\<forall>bound_x11.
         (\<forall>bound_x21.
           (\<forall>bound_y11.
             (\<forall>bound_y21.
               ((((((callOrigin bound_x11) = (callOrigin bound_x21)) \<and> ((callOrigin bound_y11) = (callOrigin bound_y21)))
                 \<and> \<not>((callOrigin bound_x11) = (callOrigin bound_y11)))
                 \<and> (bound_x21 \<in> (happensBefore bound_y11)))
                 \<longrightarrow> (bound_x21 \<in> (happensBefore bound_y21)))))))"

assumes before_procedure_invocation_no_invocation_implies_no_result_2:

"(\<forall>bound_i12. (((invocationOp bound_i12) = no_invocation) \<longrightarrow> ((invocationRes bound_i12) = NoReturn)))"

assumes before_procedure_invocation_happensBefore_antisym:

"(\<forall>bound_x5.
         (\<forall>bound_y5. (((bound_x5 \<in> (happensBefore bound_y5)) \<and> (bound_y5 \<in> (happensBefore bound_x5))) \<longrightarrow> (bound_x5 = bound_y5))))"

assumes before_procedure_invocation_happensBefore_trans:

"(\<forall>bound_x4.
         (\<forall>bound_y4.
           (\<forall>bound_z.
             (((bound_x4 \<in> (happensBefore bound_y4)) \<and> (bound_y4 \<in> (happensBefore bound_z)))
               \<longrightarrow> (bound_x4 \<in> (happensBefore bound_y4))))))"

assumes before_procedure_invocation_happensBefore_reflex:

"(\<forall>bound_c30. (((calls bound_c30) \<noteq> no_call) \<longrightarrow> (bound_c30 \<in> (happensBefore bound_c30))))"

assumes before_procedure_invocation_visibleCalls_causally_consistent:

"(\<forall>bound_c114. (\<forall>bound_c29. (((bound_c29 \<in> {}) \<and> (bound_c114 \<in> (happensBefore bound_c29))) \<longrightarrow> (bound_c114 \<in> {}))))"

assumes before_procedure_invocation_visibleCalls_transaction_consistent1:

"(\<forall>bound_c113.
         (\<forall>bound_c28.
           ((((bound_c113 \<in> {}) \<and> ((callOrigin bound_c113) = (callOrigin bound_c28))) \<and> ((calls bound_c28) \<noteq> no_call))
             \<longrightarrow> (bound_c28 \<in> {}))))"

assumes before_procedure_invocation_visibleCalls_exist:

"(\<forall>bound_c20. ((bound_c20 \<in> {}) \<longrightarrow> ((calls bound_c20) \<noteq> no_call)))"

assumes before_procedure_invocation_invocation_sequential:

"(\<forall>bound_c112.
         (\<forall>bound_tx11.
           (\<forall>bound_i11.
             (\<forall>bound_c27.
               (\<forall>bound_tx24.
                 ((((((callOrigin bound_c112) = (Some bound_tx11)) \<and> ((transactionOrigin bound_tx11) = (Some bound_i11)))
                   \<and> ((callOrigin bound_c27) = (Some bound_tx24)))
                   \<and> ((transactionOrigin bound_tx24) = (Some bound_i11)))
                   \<longrightarrow> ((bound_c112 \<in> (happensBefore bound_c27)) \<or> (bound_c27 \<in> (happensBefore bound_c112)))))))))"

assumes before_procedure_invocation_happensBefore_exists_r:

"(\<forall>bound_c111. (\<forall>bound_c26. (((calls bound_c111) = no_call) \<longrightarrow> \<not>(bound_c111 \<in> (happensBefore bound_c26)))))"

assumes before_procedure_invocation_happensBefore_exists_l:

"(\<forall>bound_c110. (\<forall>bound_c25. (((calls bound_c110) = no_call) \<longrightarrow> \<not>(bound_c110 \<in> (happensBefore bound_c25)))))"

assumes no_call_in_new_invocation:

"((invocationCalls currentInvocation) = {})"

assumes no_transaction_in_new_invocation:

"(\<forall>tx. ((transactionOrigin tx) \<noteq> (Some currentInvocation)))"

assumes before_procedure_invocation_invariant_13:

"(\<forall>bound_c19.
         (\<forall>bound_m6.
           (((calls bound_c19) = (chat_add bound_m6))
             \<longrightarrow> (\<exists>bound_i10.
               (((case (callOrigin bound_c19) of    Some bound_tx8 => (transactionOrigin bound_tx8) | None => None)
                 = (Some bound_i10))
                 \<and> (\<exists>bound_x3. (\<exists>bound_y3. ((invocationOp bound_i10) = (sendMessage bound_x3 bound_y3)))))))))"

assumes before_procedure_invocation_invariant_12:

"(\<forall>bound_c18.
         (\<forall>bound_m5.
           (\<forall>bound_s4.
             (((calls bound_c18) = (message_content_assign bound_m5 bound_s4))
               \<longrightarrow> (\<exists>bound_i9.
                 (((case (callOrigin bound_c18) of    Some bound_tx7 => (transactionOrigin bound_tx7) | None => None)
                   = (Some bound_i9))
                   \<and> (\<exists>bound_x1.
                     (\<exists>bound_y1.
                       (((invocationOp bound_i9) = (sendMessage bound_x1 bound_y1))
                         \<or> (\<exists>bound_x2. (\<exists>bound_y2. ((invocationOp bound_i9) = (editMessage bound_x2 bound_y2)))))))))))))"

assumes before_procedure_invocation_invariant_11:

"(\<forall>bound_c10.
         (\<forall>bound_m4.
           (\<forall>bound_u3.
             (((calls bound_c10) = (message_author_assign bound_m4 bound_u3))
               \<longrightarrow> (\<exists>bound_i8.
                 (((case (callOrigin bound_c10) of    Some bound_tx6 => (transactionOrigin bound_tx6) | None => None)
                   = (Some bound_i8))
                   \<and> (\<exists>bound_x. (\<exists>bound_y. ((invocationOp bound_i8) = (sendMessage bound_x bound_y))))))))))"

assumes before_procedure_invocation_invariant_10:

"(\<forall>bound_i7.
         (\<forall>bound_id5.
           (((invocationOp bound_i7) = (getMessage bound_id5))
             \<longrightarrow> (((invocationRes bound_i7) = (NoResult )) \<or> (\<exists>bound_r1. ((invocationRes bound_i7) = (getMessage_res bound_r1)))))))"

assumes before_procedure_invocation_invariant_9:

"(\<forall>bound_i6.
         (\<forall>bound_id4.
           (((invocationOp bound_i6) = (deleteMessage bound_id4))
             \<longrightarrow> (((invocationRes bound_i6) = (NoResult )) \<or> ((invocationRes bound_i6) = (deleteMessage_res ))))))"

assumes before_procedure_invocation_invariant_8:

"(\<forall>bound_i5.
         (\<forall>bound_id3.
           (\<forall>bound_newContent1.
             (((invocationOp bound_i5) = (editMessage bound_id3 bound_newContent1))
               \<longrightarrow> (((invocationRes bound_i5) = (NoResult )) \<or> ((invocationRes bound_i5) = (editMessage_res )))))))"

assumes before_procedure_invocation_invariant_7:

"(\<forall>bound_i4.
         (\<forall>bound_from1.
           (\<forall>bound_content3.
             (((invocationOp bound_i4) = (sendMessage bound_from1 bound_content3))
               \<longrightarrow> (((invocationRes bound_i4) = (NoResult )) \<or> (\<exists>bound_r. ((invocationRes bound_i4) = (sendMessage_res bound_r))))))))"

assumes before_procedure_invocation_invariant_6:

"(\<forall>bound_i3.
         (\<forall>bound_id2.
           (((invocationOp bound_i3) = (getMessage bound_id2))
             \<longrightarrow> (\<forall>bound_tx5.
               (((transactionOrigin bound_tx5) = (Some bound_i3))
                 \<longrightarrow> ((\<forall>bound_tx23. (((transactionOrigin bound_tx23) = (Some bound_i3)) \<longrightarrow> (bound_tx5 = bound_tx23)))
                   \<and> ((\<exists>bound_c16.
                     (\<exists>bound_c24.
                       (\<exists>bound_c32.
                         (\<exists>bound_u2.
                           (\<exists>bound_s3.
                             ((((((((((callOrigin bound_c16) = (Some bound_tx5))
                               \<and> ((calls bound_c16) = (queryop_message_exists bound_id2 true)))
                               \<and> (bound_c16 \<in> (happensBefore bound_c24)))
                               \<and> ((callOrigin bound_c24) = (Some bound_tx5)))
                               \<and> ((calls bound_c24) = (queryop_message_author_get bound_id2 bound_u2)))
                               \<and> (bound_c24 \<in> (happensBefore bound_c32)))
                               \<and> ((callOrigin bound_c32) = (Some bound_tx5)))
                               \<and> ((calls bound_c32) = (queryop_message_content_getFirst bound_id2 bound_s3)))
                               \<and> (\<forall>bound_c8.
                                 (((callOrigin bound_c8) = (Some bound_tx5))
                                   \<longrightarrow> (((bound_c8 = bound_c16) \<or> (bound_c8 = bound_c24)) \<or> (bound_c8 = bound_c32))))))))))
                     \<or> (\<exists>bound_c17.
                       ((((callOrigin bound_c17) = (Some bound_tx5))
                         \<and> ((calls bound_c17) = (queryop_message_exists bound_id2 false)))
                         \<and> (\<forall>bound_c9. (((callOrigin bound_c9) = (Some bound_tx5)) \<longrightarrow> (bound_c9 = bound_c17))))))))))))"

assumes before_procedure_invocation_invariant_5:

"(\<forall>bound_i2.
         (\<forall>bound_id1.
           (((invocationOp bound_i2) = (deleteMessage bound_id1))
             \<longrightarrow> (\<forall>bound_tx4.
               (((transactionOrigin bound_tx4) = (Some bound_i2))
                 \<longrightarrow> ((\<forall>bound_tx22. (((transactionOrigin bound_tx22) = (Some bound_i2)) \<longrightarrow> (bound_tx4 = bound_tx22)))
                   \<and> ((\<exists>bound_c14.
                     (\<exists>bound_c23.
                       (\<exists>bound_c31.
                         ((((((((((callOrigin bound_c14) = (Some bound_tx4))
                           \<and> ((calls bound_c14) = (queryop_message_exists bound_id1 true)))
                           \<and> (bound_c14 \<in> (happensBefore bound_c23)))
                           \<and> ((callOrigin bound_c23) = (Some bound_tx4)))
                           \<and> ((calls bound_c23) = (chat_remove bound_id1)))
                           \<and> (bound_c23 \<in> (happensBefore bound_c31)))
                           \<and> ((callOrigin bound_c31) = (Some bound_tx4)))
                           \<and> ((calls bound_c31) = (message_delete bound_id1)))
                           \<and> (\<forall>bound_c6.
                             (((callOrigin bound_c6) = (Some bound_tx4))
                               \<longrightarrow> (((bound_c6 = bound_c14) \<or> (bound_c6 = bound_c23)) \<or> (bound_c6 = bound_c31))))))))
                     \<or> (\<exists>bound_c15.
                       ((((callOrigin bound_c15) = (Some bound_tx4))
                         \<and> ((calls bound_c15) = (queryop_message_exists bound_id1 false)))
                         \<and> (\<forall>bound_c7. (((callOrigin bound_c7) = (Some bound_tx4)) \<longrightarrow> (bound_c7 = bound_c15))))))))))))"

assumes before_procedure_invocation_invariant_4:

"(\<forall>bound_i1.
         (\<forall>bound_id.
           (\<forall>bound_newContent.
             (((invocationOp bound_i1) = (editMessage bound_id bound_newContent))
               \<longrightarrow> (\<forall>bound_tx3.
                 (((transactionOrigin bound_tx3) = (Some bound_i1))
                   \<longrightarrow> ((\<forall>bound_tx21. (((transactionOrigin bound_tx21) = (Some bound_i1)) \<longrightarrow> (bound_tx3 = bound_tx21)))
                     \<and> ((\<exists>bound_c12.
                       (\<exists>bound_c22.
                         (((((((callOrigin bound_c12) = (Some bound_tx3))
                           \<and> ((calls bound_c12) = (queryop_message_exists bound_id true)))
                           \<and> (bound_c12 \<in> (happensBefore bound_c22)))
                           \<and> ((callOrigin bound_c22) = (Some bound_tx3)))
                           \<and> ((calls bound_c22) = (message_content_assign bound_id bound_newContent)))
                           \<and> (\<forall>bound_c4.
                             (((callOrigin bound_c4) = (Some bound_tx3)) \<longrightarrow> ((bound_c4 = bound_c12) \<or> (bound_c4 = bound_c22)))))))
                       \<or> (\<exists>bound_c13.
                         ((((callOrigin bound_c13) = (Some bound_tx3))
                           \<and> ((calls bound_c13) = (queryop_message_exists bound_id false)))
                           \<and> (\<forall>bound_c5. (((callOrigin bound_c5) = (Some bound_tx3)) \<longrightarrow> (bound_c5 = bound_c13)))))))))))))"

assumes before_procedure_invocation_invariant_3:

"(\<forall>bound_i.
         (\<forall>bound_from.
           (\<forall>bound_content1.
             (((invocationOp bound_i) = (sendMessage bound_from bound_content1))
               \<longrightarrow> (\<forall>bound_tx1.
                 (((transactionOrigin bound_tx1) = (Some bound_i))
                   \<longrightarrow> ((\<forall>bound_tx2. (((transactionOrigin bound_tx2) = (Some bound_i)) \<longrightarrow> (bound_tx1 = bound_tx2)))
                     \<and> (\<exists>bound_c11.
                       (\<exists>bound_c21.
                         (\<exists>bound_c3.
                           (\<exists>bound_m3.
                             ((((((((((callOrigin bound_c11) = (Some bound_tx1))
                               \<and> ((calls bound_c11) = (message_author_assign bound_m3 bound_from)))
                               \<and> (bound_c11 \<in> (happensBefore bound_c21)))
                               \<and> ((callOrigin bound_c21) = (Some bound_tx1)))
                               \<and> ((calls bound_c21) = (message_content_assign bound_m3 bound_content1)))
                               \<and> (bound_c21 \<in> (happensBefore bound_c3)))
                               \<and> ((callOrigin bound_c3) = (Some bound_tx1)))
                               \<and> ((calls bound_c3) = (chat_add bound_m3)))
                               \<and> (\<forall>bound_c.
                                 (((callOrigin bound_c) = (Some bound_tx1))
                                   \<longrightarrow> (((bound_c = bound_c11) \<or> (bound_c = bound_c21)) \<or> (bound_c = bound_c3))))))))))))))))"

assumes before_procedure_invocation_invariant_2:

"\<not>(\<exists>bound_write.
         (\<exists>bound_delete.
           (\<exists>bound_m2.
             ((((\<exists>bound_u1. ((calls bound_write) = (message_author_assign bound_m2 bound_u1)))
               \<or> (\<exists>bound_s2. ((calls bound_write) = (message_content_assign bound_m2 bound_s2))))
               \<and> ((calls bound_delete) = (message_delete bound_m2)))
               \<and> (bound_delete \<in> (happensBefore bound_write))))))"

assumes before_procedure_invocation_invariant_1:

"(\<forall>bound_c1.
         (\<forall>bound_m1.
           (\<forall>bound_s1.
             (((calls bound_c1) = (message_content_assign bound_m1 bound_s1))
               \<longrightarrow> (\<exists>bound_c2.
                 (\<exists>bound_u.
                   (((calls bound_c2) = (message_author_assign bound_m1 bound_u)) \<and> (bound_c2 \<in> (happensBefore bound_c1)))))))))"

assumes before_procedure_invocation_invariant_0:

"(\<forall>bound_g.
         (\<forall>bound_m.
           (\<forall>bound_author.
             (\<forall>bound_content.
               ((((invocationOp bound_g) = (getMessage bound_m))
                 \<and> ((invocationRes bound_g) = (getMessage_res (found bound_author bound_content))))
                 \<longrightarrow> (\<exists>bound_s. (\<exists>bound_content2. ((invocationOp bound_s) = (sendMessage bound_author bound_content2)))))))))"

assumes i_fresh:

"((invocationOp currentInvocation) = no_invocation)"

assumes old_transactions_unchanged:

"(\<forall>c9.
         (\<forall>tx4.
           (((((calls c9) = no_call) \<and> ((calls1 c9) \<noteq> no_call)) \<and> ((callOrigin1 c9) = (Some tx4)))
             \<longrightarrow> ((transactionOrigin tx4) = None))))"

assumes growth_invocation_res:

"(\<forall>i5. (((invocationRes i5) \<noteq> NoReturn) \<longrightarrow> ((invocationRes i5) = (invocationRes i5))))"

assumes growth_invocation_op:

"(\<forall>i4.
         ((((invocationOp(currentInvocation := (getMessage m_init))) i4) \<noteq> no_invocation)
           \<longrightarrow> (((invocationOp(currentInvocation := (getMessage m_init))) i4)
             = ((invocationOp(currentInvocation := (getMessage m_init))) i4))))"

assumes growth_tx_origin:

"(\<forall>tx3. (\<not>((transactionOrigin tx3) = None) \<longrightarrow> ((transactionOrigin1 tx3) = (transactionOrigin tx3))))"

assumes growth_call_tx:

"(\<forall>c8. (((calls c8) \<noteq> no_call) \<longrightarrow> ((callOrigin1 c8) = (callOrigin c8))))"

assumes growth_happensbefore:

"(\<forall>c7. (((calls c7) \<noteq> no_call) \<longrightarrow> ((happensBefore1 c7) = (happensBefore c7))))"

assumes growth_calls:

"(\<forall>c6. (((calls c6) \<noteq> no_call) \<longrightarrow> ((calls1 c6) = (calls c6))))"

assumes growth_visible_calls:

"(\<forall>c5. ((c5 \<in> {}) \<longrightarrow> (c5 \<in> {})))"

assumes growth_callOrigin:

"(\<forall>c4. (\<forall>tx2. (((callOrigin c4) = (Some tx2)) \<longrightarrow> ((callOrigin1 c4) = (Some tx2)))))"

assumes transaction_begin_snapshot_addition_transaction_consistent:

"(\<forall>bound_c131.
         (\<forall>bound_c224.
           (((bound_c131 \<in> snapshotAddition1) \<and> (bound_c224 \<in> (happensBefore1 bound_c131))) \<longrightarrow> (bound_c224 \<in> snapshotAddition1))))"

assumes transaction_begin_snapshot_addition_transaction_consistent_2:

"(\<forall>bound_c130.
         (\<forall>bound_c223.
           (((bound_c130 \<in> snapshotAddition1) \<and> ((callOrigin1 bound_c130) = (callOrigin1 bound_c223)))
             \<longrightarrow> (bound_c223 \<in> snapshotAddition1))))"

assumes transaction_begin_snapshot_addition_subset_calls:

"(\<forall>bound_c64. ((bound_c64 \<in> snapshotAddition1) \<longrightarrow> ((calls1 bound_c64) \<noteq> no_call)))"

assumes transaction_begin_MessageId_knownIds_are_generated:

"(\<forall>bound_x19. ((bound_x19 \<in> knownIds_MessageId1) \<longrightarrow> \<not>((generatedIds_MessageId1 bound_x19) = None)))"

assumes transaction_begin_message_delete_call_parameter_key_generated:

"(\<forall>bound_c63.
         (\<forall>bound_key5. (((calls1 bound_c63) = (message_delete bound_key5)) \<longrightarrow> \<not>((generatedIds_MessageId1 bound_key5) = None))))"

assumes transaction_begin_message_content_assign_call_parameter_key_generated:

"(\<forall>bound_c62.
         (\<forall>bound_key4.
           (\<forall>bound_value3.
             (((calls1 bound_c62) = (message_content_assign bound_key4 bound_value3))
               \<longrightarrow> \<not>((generatedIds_MessageId1 bound_key4) = None)))))"

assumes transaction_begin_message_author_assign_call_parameter_key_generated:

"(\<forall>bound_c61.
         (\<forall>bound_key3.
           (\<forall>bound_value2.
             (((calls1 bound_c61) = (message_author_assign bound_key3 bound_value2))
               \<longrightarrow> \<not>((generatedIds_MessageId1 bound_key3) = None)))))"

assumes transaction_begin_chat_remove_call_parameter_elem_generated:

"(\<forall>bound_c60.
         (\<forall>bound_elem3. (((calls1 bound_c60) = (chat_remove bound_elem3)) \<longrightarrow> \<not>((generatedIds_MessageId1 bound_elem3) = None))))"

assumes transaction_begin_chat_add_call_parameter_elem_generated:

"(\<forall>bound_c59.
         (\<forall>bound_elem2. (((calls1 bound_c59) = (chat_add bound_elem2)) \<longrightarrow> \<not>((generatedIds_MessageId1 bound_elem2) = None))))"

assumes transaction_begin_sendMessage_result_known:

"(\<forall>bound_i40.
         (\<forall>bound_result1.
           (((invocationRes bound_i40) = (sendMessage_res bound_result1)) \<longrightarrow> (bound_result1 \<in> knownIds_MessageId1))))"

assumes transaction_begin_getMessage_parameter_m_known:

"(\<forall>bound_i39.
         (\<forall>bound_m16.
           ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i39) = (getMessage bound_m16))
             \<longrightarrow> (bound_m16 \<in> knownIds_MessageId1))))"

assumes transaction_begin_deleteMessage_parameter_message_id_known:

"(\<forall>bound_i38.
         (\<forall>bound_message_id1.
           ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i38) = (deleteMessage bound_message_id1))
             \<longrightarrow> (bound_message_id1 \<in> knownIds_MessageId1))))"

assumes transaction_begin_editMessage_parameter_id_known:

"(\<forall>bound_i37.
         (\<forall>bound_id13.
           (\<forall>bound_newContent5.
             ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i37)
               = (editMessage bound_id13 bound_newContent5))
               \<longrightarrow> (bound_id13 \<in> knownIds_MessageId1)))))"

assumes transaction_begin_WF_transactionOrigin_exists:

"(\<forall>bound_tx35.
         (\<forall>bound_i36.
           (((transactionOrigin1 bound_tx35) = (Some bound_i36))
             \<longrightarrow> (((invocationOp(currentInvocation := (getMessage m_init))) bound_i36) \<noteq> no_invocation))))"

assumes transaction_begin_WF_callOrigin_exists:

"(\<forall>bound_ca3.
         (\<forall>bound_tx34. (((callOrigin1 bound_ca3) = (Some bound_tx34)) \<longrightarrow> \<not>((transactionOrigin1 bound_tx34) = None))))"

assumes transaction_begin_WF_no_call_implies_not_in_happensBefore:

"(\<forall>bound_ca2. (\<forall>bound_cb1. (((callOrigin1 bound_ca2) = None) \<longrightarrow> \<not>(bound_ca2 \<in> (happensBefore1 bound_cb1)))))"

assumes transaction_begin_WF_no_call_implies_no_happensBefore:

"(\<forall>bound_c58. (((callOrigin1 bound_c58) = None) \<longrightarrow> ((happensBefore1 bound_c58) = {})))"

assumes transaction_begin_WF_transactionOrigin_callOrigin:

"(\<forall>bound_tx33. (((transactionOrigin1 bound_tx33) = None) \<longrightarrow> (\<forall>bound_c57. ((callOrigin1 bound_c57) \<noteq> (Some bound_tx33)))))"

assumes transaction_begin_WF_callOrigin:

"(\<forall>bound_c56. (((callOrigin1 bound_c56) = None) = ((calls1 bound_c56) = no_call)))"

assumes transaction_begin_WF_invocationCalls:

"(\<forall>bound_i35.
         (\<forall>bound_c55.
           ((bound_c55 \<in> (invocationCalls1 bound_i35))
             = (\<exists>bound_tx32.
             (((callOrigin1 bound_c55) = (Some bound_tx32)) \<and> ((transactionOrigin1 bound_tx32) = (Some bound_i35)))))))"

assumes transaction_begin_no_invocation_implies_no_result:

"(\<forall>bound_x18.
         (\<forall>bound_x22.
           (\<forall>bound_y19.
             (\<forall>bound_y22.
               ((((((callOrigin1 bound_x18) = (callOrigin1 bound_x22)) \<and> ((callOrigin1 bound_y19) = (callOrigin1 bound_y22)))
                 \<and> \<not>((callOrigin1 bound_x18) = (callOrigin1 bound_y19)))
                 \<and> (bound_x22 \<in> (happensBefore1 bound_y19)))
                 \<longrightarrow> (bound_x22 \<in> (happensBefore1 bound_y22)))))))"

assumes transaction_begin_no_invocation_implies_no_result_2:

"(\<forall>bound_i34.
         ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i34) = no_invocation)
           \<longrightarrow> ((invocationRes bound_i34) = NoReturn)))"

assumes transaction_begin_happensBefore_antisym:

"(\<forall>bound_x17.
         (\<forall>bound_y17.
           (((bound_x17 \<in> (happensBefore1 bound_y17)) \<and> (bound_y17 \<in> (happensBefore1 bound_x17))) \<longrightarrow> (bound_x17 = bound_y17))))"

assumes transaction_begin_happensBefore_trans:

"(\<forall>bound_x16.
         (\<forall>bound_y16.
           (\<forall>bound_z1.
             (((bound_x16 \<in> (happensBefore1 bound_y16)) \<and> (bound_y16 \<in> (happensBefore1 bound_z1)))
               \<longrightarrow> (bound_x16 \<in> (happensBefore1 bound_y16))))))"

assumes transaction_begin_happensBefore_reflex:

"(\<forall>bound_c54. (((calls1 bound_c54) \<noteq> no_call) \<longrightarrow> (bound_c54 \<in> (happensBefore1 bound_c54))))"

assumes transaction_begin_visibleCalls_causally_consistent:

"(\<forall>bound_c129. (\<forall>bound_c222. (((bound_c222 \<in> vis) \<and> (bound_c129 \<in> (happensBefore1 bound_c222))) \<longrightarrow> (bound_c129 \<in> vis))))"

assumes transaction_begin_visibleCalls_transaction_consistent1:

"(\<forall>bound_c128.
         (\<forall>bound_c221.
           ((((bound_c128 \<in> vis) \<and> ((callOrigin1 bound_c128) = (callOrigin1 bound_c221))) \<and> ((calls1 bound_c221) \<noteq> no_call))
             \<longrightarrow> (bound_c221 \<in> vis))))"

assumes transaction_begin_visibleCalls_exist:

"(\<forall>bound_c53. ((bound_c53 \<in> vis) \<longrightarrow> ((calls1 bound_c53) \<noteq> no_call)))"

assumes transaction_begin_invocation_sequential:

"(\<forall>bound_c127.
         (\<forall>bound_tx110.
           (\<forall>bound_i33.
             (\<forall>bound_c220.
               (\<forall>bound_tx210.
                 ((((((callOrigin1 bound_c127) = (Some bound_tx110)) \<and> ((transactionOrigin1 bound_tx110) = (Some bound_i33)))
                   \<and> ((callOrigin1 bound_c220) = (Some bound_tx210)))
                   \<and> ((transactionOrigin1 bound_tx210) = (Some bound_i33)))
                   \<longrightarrow> ((bound_c127 \<in> (happensBefore1 bound_c220)) \<or> (bound_c220 \<in> (happensBefore1 bound_c127)))))))))"

assumes transaction_begin_happensBefore_exists_r:

"(\<forall>bound_c126. (\<forall>bound_c219. (((calls1 bound_c126) = no_call) \<longrightarrow> \<not>(bound_c126 \<in> (happensBefore1 bound_c219)))))"

assumes transaction_begin_happensBefore_exists_l:

"(\<forall>bound_c125. (\<forall>bound_c218. (((calls1 bound_c125) = no_call) \<longrightarrow> \<not>(bound_c125 \<in> (happensBefore1 bound_c218)))))"

assumes at_transaction_begin_invariant_13:

"(\<forall>bound_c52.
         (\<forall>bound_m15.
           (((calls1 bound_c52) = (chat_add bound_m15))
             \<longrightarrow> (\<exists>bound_i32.
               (((case (callOrigin1 bound_c52) of    Some bound_tx31 => (transactionOrigin1 bound_tx31) | None => None)
                 = (Some bound_i32))
                 \<and> (\<exists>bound_x15.
                   (\<exists>bound_y15.
                     (((invocationOp(currentInvocation := (getMessage m_init))) bound_i32) = (sendMessage bound_x15 bound_y15)))))))))"

assumes at_transaction_begin_invariant_12:

"(\<forall>bound_c51.
         (\<forall>bound_m14.
           (\<forall>bound_s11.
             (((calls1 bound_c51) = (message_content_assign bound_m14 bound_s11))
               \<longrightarrow> (\<exists>bound_i31.
                 (((case (callOrigin1 bound_c51) of    Some bound_tx30 => (transactionOrigin1 bound_tx30) | None => None)
                   = (Some bound_i31))
                   \<and> (\<exists>bound_x13.
                     (\<exists>bound_y13.
                       ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i31)
                         = (sendMessage bound_x13 bound_y13))
                         \<or> (\<exists>bound_x14.
                           (\<exists>bound_y14.
                             (((invocationOp(currentInvocation := (getMessage m_init))) bound_i31)
                               = (editMessage bound_x14 bound_y14)))))))))))))"

assumes at_transaction_begin_invariant_11:

"(\<forall>bound_c50.
         (\<forall>bound_m13.
           (\<forall>bound_u9.
             (((calls1 bound_c50) = (message_author_assign bound_m13 bound_u9))
               \<longrightarrow> (\<exists>bound_i30.
                 (((case (callOrigin1 bound_c50) of    Some bound_tx29 => (transactionOrigin1 bound_tx29) | None => None)
                   = (Some bound_i30))
                   \<and> (\<exists>bound_x12.
                     (\<exists>bound_y12.
                       (((invocationOp(currentInvocation := (getMessage m_init))) bound_i30)
                         = (sendMessage bound_x12 bound_y12))))))))))"

assumes at_transaction_begin_invariant_10:

"(\<forall>bound_i29.
         (\<forall>bound_id12.
           ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i29) = (getMessage bound_id12))
             \<longrightarrow> (((invocationRes bound_i29) = (NoResult ))
               \<or> (\<exists>bound_r5. ((invocationRes bound_i29) = (getMessage_res bound_r5)))))))"

assumes at_transaction_begin_invariant_9:

"(\<forall>bound_i28.
         (\<forall>bound_id11.
           ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i28) = (deleteMessage bound_id11))
             \<longrightarrow> (((invocationRes bound_i28) = (NoResult )) \<or> ((invocationRes bound_i28) = (deleteMessage_res ))))))"

assumes at_transaction_begin_invariant_8:

"(\<forall>bound_i27.
         (\<forall>bound_id10.
           (\<forall>bound_newContent4.
             ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i27)
               = (editMessage bound_id10 bound_newContent4))
               \<longrightarrow> (((invocationRes bound_i27) = (NoResult )) \<or> ((invocationRes bound_i27) = (editMessage_res )))))))"

assumes at_transaction_begin_invariant_7:

"(\<forall>bound_i26.
         (\<forall>bound_from3.
           (\<forall>bound_content6.
             ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i26) = (sendMessage bound_from3 bound_content6))
               \<longrightarrow> (((invocationRes bound_i26) = (NoResult ))
                 \<or> (\<exists>bound_r4. ((invocationRes bound_i26) = (sendMessage_res bound_r4))))))))"

assumes at_transaction_begin_invariant_6:

"(\<forall>bound_i25.
         (\<forall>bound_id9.
           ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i25) = (getMessage bound_id9))
             \<longrightarrow> (\<forall>bound_tx20.
               (((transactionOrigin1 bound_tx20) = (Some bound_i25))
                 \<longrightarrow> ((\<forall>bound_tx28. (((transactionOrigin1 bound_tx28) = (Some bound_i25)) \<longrightarrow> (bound_tx20 = bound_tx28)))
                   \<and> ((\<exists>bound_c123.
                     (\<exists>bound_c217.
                       (\<exists>bound_c312.
                         (\<exists>bound_u8.
                           (\<exists>bound_s10.
                             ((((((((((callOrigin1 bound_c123) = (Some bound_tx20))
                               \<and> ((calls1 bound_c123) = (queryop_message_exists bound_id9 true)))
                               \<and> (bound_c123 \<in> (happensBefore1 bound_c217)))
                               \<and> ((callOrigin1 bound_c217) = (Some bound_tx20)))
                               \<and> ((calls1 bound_c217) = (queryop_message_author_get bound_id9 bound_u8)))
                               \<and> (bound_c217 \<in> (happensBefore1 bound_c312)))
                               \<and> ((callOrigin1 bound_c312) = (Some bound_tx20)))
                               \<and> ((calls1 bound_c312) = (queryop_message_content_getFirst bound_id9 bound_s10)))
                               \<and> (\<forall>bound_c48.
                                 (((callOrigin1 bound_c48) = (Some bound_tx20))
                                   \<longrightarrow> (((bound_c48 = bound_c123) \<or> (bound_c48 = bound_c217)) \<or> (bound_c48 = bound_c312))))))))))
                     \<or> (\<exists>bound_c124.
                       ((((callOrigin1 bound_c124) = (Some bound_tx20))
                         \<and> ((calls1 bound_c124) = (queryop_message_exists bound_id9 false)))
                         \<and> (\<forall>bound_c49. (((callOrigin1 bound_c49) = (Some bound_tx20)) \<longrightarrow> (bound_c49 = bound_c124))))))))))))"

assumes at_transaction_begin_invariant_5:

"(\<forall>bound_i24.
         (\<forall>bound_id8.
           ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i24) = (deleteMessage bound_id8))
             \<longrightarrow> (\<forall>bound_tx19.
               (((transactionOrigin1 bound_tx19) = (Some bound_i24))
                 \<longrightarrow> ((\<forall>bound_tx27. (((transactionOrigin1 bound_tx27) = (Some bound_i24)) \<longrightarrow> (bound_tx19 = bound_tx27)))
                   \<and> ((\<exists>bound_c121.
                     (\<exists>bound_c216.
                       (\<exists>bound_c311.
                         ((((((((((callOrigin1 bound_c121) = (Some bound_tx19))
                           \<and> ((calls1 bound_c121) = (queryop_message_exists bound_id8 true)))
                           \<and> (bound_c121 \<in> (happensBefore1 bound_c216)))
                           \<and> ((callOrigin1 bound_c216) = (Some bound_tx19)))
                           \<and> ((calls1 bound_c216) = (chat_remove bound_id8)))
                           \<and> (bound_c216 \<in> (happensBefore1 bound_c311)))
                           \<and> ((callOrigin1 bound_c311) = (Some bound_tx19)))
                           \<and> ((calls1 bound_c311) = (message_delete bound_id8)))
                           \<and> (\<forall>bound_c46.
                             (((callOrigin1 bound_c46) = (Some bound_tx19))
                               \<longrightarrow> (((bound_c46 = bound_c121) \<or> (bound_c46 = bound_c216)) \<or> (bound_c46 = bound_c311))))))))
                     \<or> (\<exists>bound_c122.
                       ((((callOrigin1 bound_c122) = (Some bound_tx19))
                         \<and> ((calls1 bound_c122) = (queryop_message_exists bound_id8 false)))
                         \<and> (\<forall>bound_c47. (((callOrigin1 bound_c47) = (Some bound_tx19)) \<longrightarrow> (bound_c47 = bound_c122))))))))))))"

assumes at_transaction_begin_invariant_4:

"(\<forall>bound_i23.
         (\<forall>bound_id7.
           (\<forall>bound_newContent3.
             ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i23) = (editMessage bound_id7 bound_newContent3))
               \<longrightarrow> (\<forall>bound_tx18.
                 (((transactionOrigin1 bound_tx18) = (Some bound_i23))
                   \<longrightarrow> ((\<forall>bound_tx26. (((transactionOrigin1 bound_tx26) = (Some bound_i23)) \<longrightarrow> (bound_tx18 = bound_tx26)))
                     \<and> ((\<exists>bound_c119.
                       (\<exists>bound_c215.
                         (((((((callOrigin1 bound_c119) = (Some bound_tx18))
                           \<and> ((calls1 bound_c119) = (queryop_message_exists bound_id7 true)))
                           \<and> (bound_c119 \<in> (happensBefore1 bound_c215)))
                           \<and> ((callOrigin1 bound_c215) = (Some bound_tx18)))
                           \<and> ((calls1 bound_c215) = (message_content_assign bound_id7 bound_newContent3)))
                           \<and> (\<forall>bound_c44.
                             (((callOrigin1 bound_c44) = (Some bound_tx18))
                               \<longrightarrow> ((bound_c44 = bound_c119) \<or> (bound_c44 = bound_c215)))))))
                       \<or> (\<exists>bound_c120.
                         ((((callOrigin1 bound_c120) = (Some bound_tx18))
                           \<and> ((calls1 bound_c120) = (queryop_message_exists bound_id7 false)))
                           \<and> (\<forall>bound_c45. (((callOrigin1 bound_c45) = (Some bound_tx18)) \<longrightarrow> (bound_c45 = bound_c120)))))))))))))"

assumes at_transaction_begin_invariant_3:

"(\<forall>bound_i22.
         (\<forall>bound_from2.
           (\<forall>bound_content5.
             ((((invocationOp(currentInvocation := (getMessage m_init))) bound_i22) = (sendMessage bound_from2 bound_content5))
               \<longrightarrow> (\<forall>bound_tx17.
                 (((transactionOrigin1 bound_tx17) = (Some bound_i22))
                   \<longrightarrow> ((\<forall>bound_tx25. (((transactionOrigin1 bound_tx25) = (Some bound_i22)) \<longrightarrow> (bound_tx17 = bound_tx25)))
                     \<and> (\<exists>bound_c118.
                       (\<exists>bound_c214.
                         (\<exists>bound_c310.
                           (\<exists>bound_m12.
                             ((((((((((callOrigin1 bound_c118) = (Some bound_tx17))
                               \<and> ((calls1 bound_c118) = (message_author_assign bound_m12 bound_from2)))
                               \<and> (bound_c118 \<in> (happensBefore1 bound_c214)))
                               \<and> ((callOrigin1 bound_c214) = (Some bound_tx17)))
                               \<and> ((calls1 bound_c214) = (message_content_assign bound_m12 bound_content5)))
                               \<and> (bound_c214 \<in> (happensBefore1 bound_c310)))
                               \<and> ((callOrigin1 bound_c310) = (Some bound_tx17)))
                               \<and> ((calls1 bound_c310) = (chat_add bound_m12)))
                               \<and> (\<forall>bound_c43.
                                 (((callOrigin1 bound_c43) = (Some bound_tx17))
                                   \<longrightarrow> (((bound_c43 = bound_c118) \<or> (bound_c43 = bound_c214)) \<or> (bound_c43 = bound_c310))))))))))))))))"

assumes at_transaction_begin_invariant_2:

"\<not>(\<exists>bound_write2.
         (\<exists>bound_delete2.
           (\<exists>bound_m11.
             ((((\<exists>bound_u7. ((calls1 bound_write2) = (message_author_assign bound_m11 bound_u7)))
               \<or> (\<exists>bound_s9. ((calls1 bound_write2) = (message_content_assign bound_m11 bound_s9))))
               \<and> ((calls1 bound_delete2) = (message_delete bound_m11)))
               \<and> (bound_delete2 \<in> (happensBefore1 bound_write2))))))"

assumes at_transaction_begin_invariant_1:

"(\<forall>bound_c117.
         (\<forall>bound_m10.
           (\<forall>bound_s8.
             (((calls1 bound_c117) = (message_content_assign bound_m10 bound_s8))
               \<longrightarrow> (\<exists>bound_c213.
                 (\<exists>bound_u6.
                   (((calls1 bound_c213) = (message_author_assign bound_m10 bound_u6))
                     \<and> (bound_c213 \<in> (happensBefore1 bound_c117)))))))))"

assumes at_transaction_begin_invariant_0:

"(\<forall>bound_g1.
         (\<forall>bound_m9.
           (\<forall>bound_author1.
             (\<forall>bound_content4.
               (((((invocationOp(currentInvocation := (getMessage m_init))) bound_g1) = (getMessage bound_m9))
                 \<and> ((invocationRes bound_g1) = (getMessage_res (found bound_author1 bound_content4))))
                 \<longrightarrow> (\<exists>bound_s7.
                   (\<exists>bound_content22.
                     (((invocationOp(currentInvocation := (getMessage m_init))) bound_s7)
                       = (sendMessage bound_author1 bound_content22)))))))))"

assumes no_new_calls_addded_to_current:

"((invocationCalls currentInvocation) = (invocationCalls1 currentInvocation))"

assumes no_new_transactions_added_to_current:

"(\<forall>bound_t.
         (((transactionOrigin bound_t) = (Some currentInvocation)) = ((transactionOrigin1 bound_t) = (Some currentInvocation))))"

assumes tx1_fresh:

"((transactionOrigin1 tx1) = None)"

assumes vis_update:

"(vis = ({} \<union> newCalls))"

assumes new_transactions_exist:

"(newTxns \<subseteq> (dom transactionOrigin1))"

assumes message_exists_res_def:

"(message_exists_res
         = (\<exists>bound_c132.
         ((((bound_c132 \<in> vis) \<and> (\<exists>bound_args. ((calls1 bound_c132) = (message_author_assign m_init bound_args))))
           \<or> ((bound_c132 \<in> vis) \<and> (\<exists>bound_args1. ((calls1 bound_c132) = (message_content_assign m_init bound_args1)))))
           \<and> (\<forall>bound_c225.
             (((bound_c225 \<in> vis) \<and> ((calls1 bound_c225) = (message_delete m_init)))
               \<longrightarrow> (bound_c225 \<in> (happensBefore1 bound_c132)))))))"

assumes q__query_message_exists_res_1_assignment:

"(q__query_message_exists_res_1 = message_exists_res)"

assumes c0_freshB:

"((calls1 c0) = no_call)"

assumes c0_freshA:

"distinct [c0]"

assumes if_statement_condition_true:

"q__query_message_exists_res_1"

assumes q__query_message_author_get_res_2_assignment:

"(q__query_message_author_get_res_2 = message_author_get)"

assumes vis_def:

"(vis1 = (vis \<union> {c0}))"

assumes calls_def:

"(calls2 = (calls1(c0 := (queryop_message_exists m_init q__query_message_exists_res_1))))"

assumes happensBefore_def:

"(happensBefore2 = (happensBefore1(c0 := (vis \<union> {c0}))))"

assumes query_message_author_get_postcondition_def:

"(query_message_author_get_postcondition
         = (\<not>(\<exists>bound_c313.
         (\<exists>bound_value21.
           ((bound_c313 \<in> vis1)
             \<and> (((calls2 bound_c313) = (message_author_assign m_init bound_value21))
               \<and> (\<forall>bound_d.
                 (((bound_d \<in> vis1) \<and> ((calls2 bound_d) = (message_delete m_init))) \<longrightarrow> (bound_d \<in> (happensBefore2 bound_c313))))))))
         \<or> (\<exists>bound_c133.
           (((bound_c133 \<in> vis1)
             \<and> (((calls2 bound_c133) = (message_author_assign m_init message_author_get))
               \<and> (\<forall>bound_d1.
                 (((bound_d1 \<in> vis1) \<and> ((calls2 bound_d1) = (message_delete m_init)))
                   \<longrightarrow> (bound_d1 \<in> (happensBefore2 bound_c133))))))
             \<and> \<not>(\<exists>bound_c226.
               (\<exists>bound_value4.
                 ((((bound_c226 \<in> vis1) \<and> (bound_c133 \<noteq> bound_c226))
                   \<and> (((calls2 bound_c226) = (message_author_assign m_init bound_value4))
                     \<and> (\<forall>bound_d2.
                       (((bound_d2 \<in> vis1) \<and> ((calls2 bound_d2) = (message_delete m_init)))
                         \<longrightarrow> (bound_d2 \<in> (happensBefore2 bound_c226))))))
                   \<and> (bound_c133 \<in> (happensBefore2 bound_c226)))))))))"

assumes choose_message_author_get:

"query_message_author_get_postcondition"

assumes c11_freshB:

"((calls2 c11) = no_call)"

assumes c11_freshA:

"distinct [c11 , c0]"

assumes q__query_message_content_getFirst_res_3_assignment:

"(q__query_message_content_getFirst_res_3 = message_content_getFirst)"

assumes vis_def_2:

"(vis2 = (vis1 \<union> {c11}))"

assumes calls_def_2:

"(calls3 = (calls2(c11 := (queryop_message_author_get m_init q__query_message_author_get_res_2))))"

assumes happensBefore_def_2:

"(happensBefore3 = (happensBefore2(c11 := (vis1 \<union> {c11}))))"

assumes query_message_content_getFirst_postcondition_def:

"(query_message_content_getFirst_postcondition
         = (\<exists>bound_c134.
         (((bound_c134 \<in> vis2)
           \<and> (((calls3 bound_c134) = (message_content_assign m_init message_content_getFirst))
             \<and> (\<forall>bound_d3.
               (((bound_d3 \<in> vis2) \<and> ((calls3 bound_d3) = (message_delete m_init))) \<longrightarrow> (bound_d3 \<in> (happensBefore3 bound_c134))))))
           \<and> \<not>(\<exists>bound_c227.
             ((((bound_c227 \<in> vis2) \<and> (bound_c134 \<noteq> bound_c227))
               \<and> (((calls3 bound_c227) = (message_content_assign m_init message_content_getFirst))
                 \<and> (\<forall>bound_d4.
                   (((bound_d4 \<in> vis2) \<and> ((calls3 bound_d4) = (message_delete m_init)))
                     \<longrightarrow> (bound_d4 \<in> (happensBefore3 bound_c227))))))
               \<and> (bound_c134 \<in> (happensBefore3 bound_c227)))))))"

assumes choose_message_content_getFirst:

"query_message_content_getFirst_postcondition"

assumes c21_freshB:

"((calls3 c21) = no_call)"

assumes c21_freshA:

"distinct [c21 , c0 , c11]"

assumes invariant_not_violated:

"\<not>(((((invocationOp(currentInvocation := (getMessage m_init))) g1) = (getMessage m6))
         \<and> (((invocationRes(currentInvocation := (getMessage_res (found q__query_message_author_get_res_2
             q__query_message_content_getFirst_res_3)))) g1)
           = (getMessage_res (found author2 content4))))
         \<longrightarrow> (\<exists>bound_s12.
           (\<exists>bound_content23.
             (((invocationOp(currentInvocation := (getMessage m_init))) bound_s12) = (sendMessage author2 bound_content23)))))"
shows False
find_theorems "\<not>(_ \<longrightarrow> _)"

  using invariant_not_violated apply (rule notE)
proof (intro impI, elim conjE)
  assume a0: "(invocationOp(currentInvocation := getMessage m_init)) g1 = getMessage m6"
    and a1: "(invocationRes(currentInvocation := getMessage_res (found q__query_message_author_get_res_2 q__query_message_content_getFirst_res_3)))       g1 =      getMessage_res (found author2 content4)"

  show "\<exists>bound_s12 bound_content23. (invocationOp(currentInvocation := getMessage m_init)) bound_s12 = sendMessage author2 bound_content23"
  proof (cases "g1 = currentInvocation")
    case False
    then show ?thesis 
      using a0 a1 apply auto
      using at_transaction_begin_invariant_0 invariant_not_violated by blast
  next
    case True
    hence [simp]:" g1 = currentInvocation". 


    show ?thesis
    proof (auto cong: conj_cong)
      show "\<exists>bound_s12. bound_s12 \<noteq> currentInvocation 
         \<and> (\<exists>bound_content23. invocationOp bound_s12 = sendMessage author2 bound_content23)"
      proof -

        have a1: "g1 = currentInvocation" by simp

        have a2: "m_init = m6"
          using invariant_not_violated by auto 

        have a3: "q__query_message_author_get_res_2 = author2"
          using invariant_not_violated by auto 
        have a4: "q__query_message_content_getFirst_res_3 = content4"
          using invariant_not_violated by auto 
        have a5: "\<forall>bound_s7. bound_s7 = currentInvocation \<or> (\<forall>bound_content23. invocationOp bound_s7 \<noteq> sendMessage author2 bound_content23)"
          using invariant_not_violated by fastforce


        from if_statement_condition_true
          q__query_message_exists_res_1_assignment
          message_exists_res_def
        obtain c_assign1
          where c_assign1a: "c_assign1 \<in> vis"
            and c_assign1b: "(\<exists>x. calls1 c_assign1 = message_author_assign m_init x) 
         \<or> (\<exists>x. calls1 c_assign1 = message_content_assign m_init x)"
            and c_assign1c: "(\<forall>c. c \<in> vis \<and> calls1 c = message_delete m_init \<longrightarrow> c \<in> happensBefore1 c_assign1)"
          by auto



        obtain c_author_assign 
          where "c_author_assign \<in> vis"
            and "(\<exists>x. calls1 c_author_assign = message_author_assign m_init x)"
            and "(\<forall>c. c \<in> vis \<and> calls1 c = message_delete m_init \<longrightarrow> c \<in> happensBefore1 c_author_assign)"
          using c_assign1b proof (rule disjE; atomize_elim; auto)
          fix x
          assume a: "calls1 c_assign1 = message_author_assign m_init x"
          thus "\<exists>c_author_assign.
            c_author_assign \<in> vis \<and>
            (\<exists>x. calls1 c_author_assign = message_author_assign m_init x) \<and>
            (\<forall>c. c \<in> vis \<and> calls1 c = message_delete m_init \<longrightarrow> c \<in> happensBefore1 c_author_assign)"
            using c_assign1a c_assign1c by blast
        next
          fix x
          assume a: "calls1 c_assign1 = message_content_assign m_init x"

          find_theorems message_content_assign message_author_assign name: inv

          from at_transaction_begin_invariant_1
          obtain c_assign2 y
            where "calls1 c_assign2 = message_author_assign m_init y"
              and "c_assign2 \<in> happensBefore1 c_assign1"
            apply auto
            using a transaction_begin_happensBefore_exists_r transaction_begin_happensBefore_reflex by blast


          show "\<exists>c_author_assign.
            c_author_assign \<in> vis \<and>
            (\<exists>x. calls1 c_author_assign = message_author_assign m_init x) \<and>
            (\<forall>c. c \<in> vis \<and> calls1 c = message_delete m_init \<longrightarrow> c \<in> happensBefore1 c_author_assign)"
          proof (rule_tac x=c_assign2 in exI, auto)



            show "c_assign2 \<in> vis" (*from happens before and causal consistency *)
            proof (rule transaction_begin_visibleCalls_causally_consistent[rule_format], intro conjI)
              show " c_assign1 \<in> vis" using c_assign1a .
              show "c_assign2 \<in> happensBefore1 c_assign1" using \<open>c_assign2 \<in> happensBefore1 c_assign1\<close> .
            qed


            show "\<exists>x. calls1 c_assign2 = message_author_assign m_init x"
              by (simp add: \<open>calls1 c_assign2 = message_author_assign m_init y\<close>)

            show "\<And>c. \<lbrakk>c \<in> vis; calls1 c = message_delete m_init\<rbrakk> \<Longrightarrow> c \<in> happensBefore1 c_assign2"
              using a at_transaction_begin_invariant_2 c_assign1c by blast
          qed
        qed

        from this 
        obtain c_author_assign author
          where "c_author_assign \<in> vis"
            and "calls1 c_author_assign = message_author_assign m_init author"
            and "(\<forall>c. c \<in> vis \<and> calls1 c = message_delete m_init \<longrightarrow> c \<in> happensBefore1 c_author_assign)"
          by blast

        find_theorems q__query_message_author_get_res_2
        find_theorems message_author_get

        have "query_message_author_get_postcondition"
          by (simp add: choose_message_author_get)


        have or_elim: "A | B \<Longrightarrow> (A \<Longrightarrow> False) \<Longrightarrow> B" for A B
          by blast

        from query_message_author_get_postcondition_def
        have "((\<nexists>bound_c313 bound_value21.
       bound_c313 \<in> vis1 \<and>
       calls2 bound_c313 = message_author_assign m_init bound_value21 \<and>
       (\<forall>bound_d. bound_d \<in> vis1 \<and> calls2 bound_d = message_delete m_init \<longrightarrow> bound_d \<in> happensBefore2 bound_c313)) \<or>
   (\<exists>bound_c143.
       (bound_c143 \<in> vis1 \<and>
        calls2 bound_c143 = message_author_assign m_init message_author_get \<and>
        (\<forall>bound_d1. bound_d1 \<in> vis1 \<and> calls2 bound_d1 = message_delete m_init \<longrightarrow> bound_d1 \<in> happensBefore2 bound_c143)) \<and>
       (\<nexists>bound_c238 bound_value4.
           ((bound_c238 \<in> vis1 \<and> bound_c143 \<noteq> bound_c238) \<and>
            calls2 bound_c238 = message_author_assign m_init bound_value4 \<and>
            (\<forall>bound_d2. bound_d2 \<in> vis1 \<and> calls2 bound_d2 = message_delete m_init \<longrightarrow> bound_d2 \<in> happensBefore2 bound_c238)) \<and>
           bound_c143 \<in> happensBefore2 bound_c238)))"
          by (simp add: choose_message_author_get)
        from this 
        have "(\<exists>bound_c143.
       (bound_c143 \<in> vis1 \<and>
        calls2 bound_c143 = message_author_assign m_init message_author_get \<and>
        (\<forall>bound_d1. bound_d1 \<in> vis1 \<and> calls2 bound_d1 = message_delete m_init \<longrightarrow> bound_d1 \<in> happensBefore2 bound_c143)) \<and>
       (\<nexists>bound_c238 bound_value4.
           ((bound_c238 \<in> vis1 \<and> bound_c143 \<noteq> bound_c238) \<and>
            calls2 bound_c238 = message_author_assign m_init bound_value4 \<and>
            (\<forall>bound_d2. bound_d2 \<in> vis1 \<and> calls2 bound_d2 = message_delete m_init \<longrightarrow> bound_d2 \<in> happensBefore2 bound_c238)) \<and>
           bound_c143 \<in> happensBefore2 bound_c238))"
        proof (rule or_elim)
          assume a: "\<nexists>bound_c313 bound_value21.
       bound_c313 \<in> vis1 \<and>
       calls2 bound_c313 = message_author_assign m_init bound_value21 \<and>
       (\<forall>bound_d. bound_d \<in> vis1 \<and> calls2 bound_d = message_delete m_init \<longrightarrow> bound_d \<in> happensBefore2 bound_c313)"

          have [simp]: "c_author_assign \<noteq> c0"
            using \<open>c_author_assign \<in> vis\<close> c0_freshB transaction_begin_visibleCalls_exist by blast

          from a show False
            apply auto
            apply (drule_tac x="c_author_assign" in spec)
            apply (auto simp add: \<open>c_author_assign \<in> vis\<close> vis_def calls_def 
                \<open>\<forall>c. c \<in> vis \<and> calls1 c = message_delete m_init \<longrightarrow> c \<in> happensBefore1 c_author_assign\<close> happensBefore_def
                \<open>calls1 c_author_assign = message_author_assign m_init author\<close> split: if_splits )
            done
        qed

        from this
        obtain c_author_assign2
          where  "c_author_assign2 \<in> vis1"
            and "calls2 c_author_assign2 = message_author_assign m_init message_author_get"
            and "(\<forall>bound_d1. bound_d1 \<in> vis1 \<and> calls2 bound_d1 = message_delete m_init \<longrightarrow> bound_d1 \<in> happensBefore2 c_author_assign2)"
            and "(\<nexists>bound_c238 bound_value4.
           ((bound_c238 \<in> vis1 \<and> c_author_assign2 \<noteq> bound_c238) \<and>
            calls2 bound_c238 = message_author_assign m_init bound_value4 \<and>
            (\<forall>bound_d2. bound_d2 \<in> vis1 \<and> calls2 bound_d2 = message_delete m_init \<longrightarrow> bound_d2 \<in> happensBefore2 bound_c238)) \<and>
           c_author_assign2 \<in> happensBefore2 bound_c238)"
          by auto

(* invocation of c_author_assign2 must come from an invocation of sendMessage *)

        have "author2 = message_author_get"
          using a3 q__query_message_author_get_res_2_assignment by auto


        obtain tx where
          "callOrigin1 c_author_assign2 = Some tx"
          by (metis \<open>calls2 c_author_assign2 = message_author_assign m_init message_author_get\<close> callInfo.distinct(25) callInfo.distinct(61) calls_def domD domIff fun_upd_apply fun_upd_same transaction_begin_WF_callOrigin)

        obtain invoc where
          "transactionOrigin1 tx = Some invoc"
          by (meson \<open>callOrigin1 c_author_assign2 = Some tx\<close> domD domIff transaction_begin_WF_callOrigin_exists)


        find_theorems transactionOrigin1 name: inv

        thm at_transaction_begin_invariant_4[rule_format]
        thm at_transaction_begin_invariant_5
        thm at_transaction_begin_invariant_6
        thm at_transaction_begin_invariant_7

        have [simp]: "invoc \<noteq> currentInvocation"
          using \<open>transactionOrigin1 tx = Some invoc\<close> before_procedure_invocation_WF_transactionOrigin_exists i_fresh no_new_transactions_added_to_current by blast

        from \<open>calls2 c_author_assign2 = message_author_assign m_init message_author_get\<close>
        have "calls1 c_author_assign2 = message_author_assign m_init message_author_get"
          by (metis \<open>callOrigin1 c_author_assign2 = Some tx\<close> c0_freshB calls_def fun_upd_other option.simps(3) transaction_begin_WF_callOrigin)


        have "\<exists>content. invocationOp invoc = sendMessage author2 content"
          using at_transaction_begin_invariant_11[rule_format, OF \<open>calls1 c_author_assign2 = message_author_assign m_init message_author_get\<close>]
          apply (auto simp add: split: if_splits option.splits)
          using `callOrigin1 c_author_assign2 = Some tx` `transactionOrigin1 tx = Some invoc`
          apply auto
          apply (insert at_transaction_begin_invariant_3[rule_format, of invoc])
          apply auto
          by (metis \<open>author2 = message_author_get\<close> \<open>calls1 c_author_assign2 = message_author_assign m_init message_author_get\<close> callInfo.distinct(51) callInfo.distinct(53) callInfo.inject(2))

        with a5
        have False
          using \<open>invoc \<noteq> currentInvocation\<close> by blast
        thus ?thesis
          ..
      qed
    qed
  qed
qed
end
