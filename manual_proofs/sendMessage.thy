
theory "sendMessage"
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

lemma "sendMessage_line3":
fixes happensBefore3 :: "CallId => CallId set"
         
fixes m2 :: "MessageId"
         
fixes callOrigin4 :: "CallId => TxId option"
         
fixes vis1 :: "CallId set"
         
fixes happensBefore4 :: "CallId => CallId set"
         
fixes newCalls :: "CallId set"
         
fixes snapshotAddition :: "CallId set"
         
fixes generatedIds_MessageId :: "MessageId => InvocationId option"
         
fixes message_exists_res1 :: "MessageId => bool"
         
fixes from_init :: "UserId"
         
fixes calls4 :: "CallId => callInfo"
         
fixes knownIds_MessageId1 :: "MessageId set"
         
fixes for_snapshot_additions :: "bool"
         
fixes transactionOrigin1 :: "TxId => InvocationId option"
         
fixes c11 :: "CallId"
         
fixes tx1 :: "TxId"
         
fixes invocationRes :: "InvocationId => invocationResult"
         
fixes calls1 :: "CallId => callInfo"
         
fixes content_init :: "String"
         
fixes invocationOp :: "InvocationId => invocationInfo"
         
fixes callOrigin :: "CallId => TxId option"
         
fixes invocationCalls :: "InvocationId => CallId set"
         
fixes calls :: "CallId => callInfo"
         
fixes calls3 :: "CallId => callInfo"
         
fixes for_snapshot_additions1 :: "bool"
         
fixes vis :: "CallId set"
         
fixes chat_contains_res3 :: "MessageId => bool"
         
fixes transactionOrigin :: "TxId => InvocationId option"
         
fixes callOrigin1 :: "CallId => TxId option"
         
fixes message_exists_res3 :: "MessageId => bool"
         
fixes vis2 :: "CallId set"
         
fixes calls2 :: "CallId => callInfo"
         
fixes callOrigin2 :: "CallId => TxId option"
         
fixes c21 :: "CallId"
         
fixes for_snapshot_additions_with_visibleCalls1 :: "bool"
         
fixes happensBefore1 :: "CallId => CallId set"
         
fixes chat_contains_res1 :: "MessageId => bool"
         
fixes from2 :: "UserId"
         
fixes chat_contains_res2 :: "MessageId => bool"
         
fixes generatedIds_MessageId1 :: "MessageId => InvocationId option"
         
fixes snapshotAddition1 :: "CallId set"
         
fixes currentInvocation :: "InvocationId"
         
fixes message_exists_res2 :: "MessageId => bool"
         
fixes invocationCalls1 :: "InvocationId => CallId set"
         
fixes c0 :: "CallId"
         
fixes content5 :: "String"
         
fixes chat_contains_res :: "MessageId => bool"
         
fixes knownIds_MessageId :: "MessageId set"
         
fixes message_exists_res :: "MessageId => bool"
         
fixes happensBefore2 :: "CallId => CallId set"
         
fixes i6 :: "InvocationId"
         
fixes callOrigin3 :: "CallId => TxId option"
         
fixes for_snapshot_additions_with_visibleCalls :: "bool"
         
fixes happensBefore :: "CallId => CallId set"
         
fixes newTxns :: "TxId set"
         
assumes before_procedure_invocation_snapshot_addition_transaction_consistent:

        "(\<forall>bound_c117.
         (\<forall>bound_c214.
           (((bound_c117 \<in> snapshotAddition) \<and> (bound_c214 \<in> (happensBefore bound_c117))) \<longrightarrow> (bound_c214 \<in> snapshotAddition))))"
         
assumes before_procedure_invocation_snapshot_addition_transaction_consistent_2:

        "(\<forall>bound_c116.
         (\<forall>bound_c213.
           (((bound_c116 \<in> snapshotAddition) \<and> ((callOrigin bound_c116) = (callOrigin bound_c213)))
             \<longrightarrow> (bound_c213 \<in> snapshotAddition))))"
         
assumes before_procedure_invocation_snapshot_addition_subset_calls:

        "(\<forall>bound_c41. ((bound_c41 \<in> snapshotAddition) \<longrightarrow> ((calls bound_c41) \<noteq> no_call)))"
         
assumes before_procedure_invocation_MessageId_knownIds_are_generated:

        "(\<forall>bound_x3. ((bound_x3 \<in> knownIds_MessageId) \<longrightarrow> \<not>((generatedIds_MessageId bound_x3) = None)))"
         
assumes before_procedure_invocation_message_delete_call_parameter_key_generated:

        "(\<forall>bound_c40.
         (\<forall>bound_key2. (((calls bound_c40) = (message_delete bound_key2)) \<longrightarrow> \<not>((generatedIds_MessageId bound_key2) = None))))"
         
assumes before_procedure_invocation_message_content_assign_call_parameter_key_generated:

        "(\<forall>bound_c39.
         (\<forall>bound_key1.
           (\<forall>bound_value1.
             (((calls bound_c39) = (message_content_assign bound_key1 bound_value1))
               \<longrightarrow> \<not>((generatedIds_MessageId bound_key1) = None)))))"
         
assumes before_procedure_invocation_message_author_assign_call_parameter_key_generated:

        "(\<forall>bound_c38.
         (\<forall>bound_key.
           (\<forall>bound_value.
             (((calls bound_c38) = (message_author_assign bound_key bound_value))
               \<longrightarrow> \<not>((generatedIds_MessageId bound_key) = None)))))"
         
assumes before_procedure_invocation_chat_remove_call_parameter_elem_generated:

        "(\<forall>bound_c37.
         (\<forall>bound_elem1. (((calls bound_c37) = (chat_remove bound_elem1)) \<longrightarrow> \<not>((generatedIds_MessageId bound_elem1) = None))))"
         
assumes before_procedure_invocation_chat_add_call_parameter_elem_generated:

        "(\<forall>bound_c36.
         (\<forall>bound_elem. (((calls bound_c36) = (chat_add bound_elem)) \<longrightarrow> \<not>((generatedIds_MessageId bound_elem) = None))))"
         
assumes before_procedure_invocation_sendMessage_result_known:

        "(\<forall>bound_i11.
         (\<forall>bound_result. (((invocationRes bound_i11) = (sendMessage_res bound_result)) \<longrightarrow> (bound_result \<in> knownIds_MessageId))))"
         
assumes before_procedure_invocation_getMessage_parameter_m_known:

        "(\<forall>bound_i10. (\<forall>bound_m5. (((invocationOp bound_i10) = (getMessage bound_m5)) \<longrightarrow> (bound_m5 \<in> knownIds_MessageId))))"
         
assumes before_procedure_invocation_deleteMessage_parameter_message_id_known:

        "(\<forall>bound_i9.
         (\<forall>bound_message_id.
           (((invocationOp bound_i9) = (deleteMessage bound_message_id)) \<longrightarrow> (bound_message_id \<in> knownIds_MessageId))))"
         
assumes before_procedure_invocation_editMessage_parameter_id_known:

        "(\<forall>bound_i8.
         (\<forall>bound_id3.
           (\<forall>bound_newContent1.
             (((invocationOp bound_i8) = (editMessage bound_id3 bound_newContent1)) \<longrightarrow> (bound_id3 \<in> knownIds_MessageId)))))"
         
assumes before_procedure_invocation_WF_transactionOrigin_exists:

        "(\<forall>bound_tx9.
         (\<forall>bound_i7. (((transactionOrigin bound_tx9) = (Some bound_i7)) \<longrightarrow> ((invocationOp bound_i7) \<noteq> no_invocation))))"
         
assumes before_procedure_invocation_WF_callOrigin_exists:

        "(\<forall>bound_ca1. (\<forall>bound_tx8. (((callOrigin bound_ca1) = (Some bound_tx8)) \<longrightarrow> \<not>((transactionOrigin bound_tx8) = None))))"
         
assumes before_procedure_invocation_WF_no_call_implies_not_in_happensBefore:

        "(\<forall>bound_ca. (\<forall>bound_cb. (((callOrigin bound_ca) = None) \<longrightarrow> \<not>(bound_ca \<in> (happensBefore bound_cb)))))"
         
assumes before_procedure_invocation_WF_no_call_implies_no_happensBefore:

        "(\<forall>bound_c35. (((callOrigin bound_c35) = None) \<longrightarrow> ((happensBefore bound_c35) = {})))"
         
assumes before_procedure_invocation_WF_transactionOrigin_callOrigin:

        "(\<forall>bound_tx7. (((transactionOrigin bound_tx7) = None) \<longrightarrow> (\<forall>bound_c34. ((callOrigin bound_c34) \<noteq> (Some bound_tx7)))))"
         
assumes before_procedure_invocation_WF_callOrigin:

        "(\<forall>bound_c33. (((callOrigin bound_c33) = None) = ((calls bound_c33) = no_call)))"
         
assumes before_procedure_invocation_WF_invocationCalls:

        "(\<forall>bound_i6.
         (\<forall>bound_c30.
           ((bound_c30 \<in> (invocationCalls bound_i6))
             = (\<exists>bound_tx6. (((callOrigin bound_c30) = (Some bound_tx6)) \<and> ((transactionOrigin bound_tx6) = (Some bound_i6)))))))"
         
assumes before_procedure_invocation_no_invocation_implies_no_result:

        "(\<forall>bound_x11.
         (\<forall>bound_x2.
           (\<forall>bound_y11.
             (\<forall>bound_y21.
               ((((((callOrigin bound_x11) = (callOrigin bound_x2)) \<and> ((callOrigin bound_y11) = (callOrigin bound_y21)))
                 \<and> \<not>((callOrigin bound_x11) = (callOrigin bound_y11)))
                 \<and> (bound_x2 \<in> (happensBefore bound_y11)))
                 \<longrightarrow> (bound_x2 \<in> (happensBefore bound_y21)))))))"
         
assumes before_procedure_invocation_no_invocation_implies_no_result_2:

        "(\<forall>bound_i5. (((invocationOp bound_i5) = no_invocation) \<longrightarrow> ((invocationRes bound_i5) = NoReturn)))"
         
assumes before_procedure_invocation_happensBefore_antisym:

        "(\<forall>bound_x1.
         (\<forall>bound_y1. (((bound_x1 \<in> (happensBefore bound_y1)) \<and> (bound_y1 \<in> (happensBefore bound_x1))) \<longrightarrow> (bound_x1 = bound_y1))))"
         
assumes before_procedure_invocation_happensBefore_trans:

        "(\<forall>bound_x.
         (\<forall>bound_y.
           (\<forall>bound_z.
             (((bound_x \<in> (happensBefore bound_y)) \<and> (bound_y \<in> (happensBefore bound_z))) \<longrightarrow> (bound_x \<in> (happensBefore bound_y))))))"
         
assumes before_procedure_invocation_happensBefore_reflex:

        "(\<forall>bound_c20. (((calls bound_c20) \<noteq> no_call) \<longrightarrow> (bound_c20 \<in> (happensBefore bound_c20))))"
         
assumes before_procedure_invocation_visibleCalls_causally_consistent:

        "(\<forall>bound_c115. (\<forall>bound_c212. (((bound_c212 \<in> {}) \<and> (bound_c115 \<in> (happensBefore bound_c212))) \<longrightarrow> (bound_c115 \<in> {}))))"
         
assumes before_procedure_invocation_visibleCalls_transaction_consistent1:

        "(\<forall>bound_c114.
         (\<forall>bound_c211.
           ((((bound_c114 \<in> {}) \<and> ((callOrigin bound_c114) = (callOrigin bound_c211))) \<and> ((calls bound_c211) \<noteq> no_call))
             \<longrightarrow> (bound_c211 \<in> {}))))"
         
assumes before_procedure_invocation_visibleCalls_exist:

        "(\<forall>bound_c10. ((bound_c10 \<in> {}) \<longrightarrow> ((calls bound_c10) \<noteq> no_call)))"
         
assumes before_procedure_invocation_invocation_sequential:

        "(\<forall>bound_c113.
         (\<forall>bound_tx11.
           (\<forall>bound_i4.
             (\<forall>bound_c210.
               (\<forall>bound_tx24.
                 ((((((callOrigin bound_c113) = (Some bound_tx11)) \<and> ((transactionOrigin bound_tx11) = (Some bound_i4)))
                   \<and> ((callOrigin bound_c210) = (Some bound_tx24)))
                   \<and> ((transactionOrigin bound_tx24) = (Some bound_i4)))
                   \<longrightarrow> ((bound_c113 \<in> (happensBefore bound_c210)) \<or> (bound_c210 \<in> (happensBefore bound_c113)))))))))"
         
assumes before_procedure_invocation_happensBefore_exists_r:

        "(\<forall>bound_c112. (\<forall>bound_c29. (((calls bound_c112) = no_call) \<longrightarrow> \<not>(bound_c112 \<in> (happensBefore bound_c29)))))"
         
assumes before_procedure_invocation_happensBefore_exists_l:

        "(\<forall>bound_c111. (\<forall>bound_c28. (((calls bound_c111) = no_call) \<longrightarrow> \<not>(bound_c111 \<in> (happensBefore bound_c28)))))"
         
assumes no_call_in_new_invocation:

        "((invocationCalls currentInvocation) = {})"
         
assumes no_transaction_in_new_invocation:

        "(\<forall>tx. ((transactionOrigin tx) \<noteq> (Some currentInvocation)))"
         
assumes before_procedure_invocation_invariant_6:

        "(\<forall>bound_i3.
         (\<forall>bound_id2.
           (((invocationOp bound_i3) = (getMessage bound_id2))
             \<longrightarrow> (\<forall>bound_tx5.
               (((transactionOrigin bound_tx5) = (Some bound_i3))
                 \<longrightarrow> ((\<forall>bound_tx23. (((transactionOrigin bound_tx23) = (Some bound_i3)) \<longrightarrow> (bound_tx5 = bound_tx23)))
                   \<and> ((\<exists>bound_c19.
                     (\<exists>bound_c27.
                       (\<exists>bound_c32.
                         (\<exists>bound_r4.
                           (((((((callOrigin bound_c19) = (Some bound_tx5)) \<and> (bound_c19 \<in> (happensBefore bound_c27)))
                             \<and> ((callOrigin bound_c27) = (Some bound_tx5)))
                             \<and> (bound_c27 \<in> (happensBefore bound_c32)))
                             \<and> ((callOrigin bound_c32) = (Some bound_tx5)))
                             \<and> (\<forall>bound_c8.
                               (((callOrigin bound_c8) = (Some bound_tx5))
                                 \<longrightarrow> (((bound_c8 = bound_c19) \<or> (bound_c8 = bound_c27)) \<or> (bound_c8 = bound_c32)))))))))
                     \<or> (\<exists>bound_c110.
                       (\<exists>bound_r5.
                         (((callOrigin bound_c110) = (Some bound_tx5))
                           \<and> (\<forall>bound_c9. (((callOrigin bound_c9) = (Some bound_tx5)) \<longrightarrow> (bound_c9 = bound_c110)))))))))))))"
         
assumes before_procedure_invocation_invariant_5:

        "(\<forall>bound_i2.
         (\<forall>bound_id1.
           (((invocationOp bound_i2) = (deleteMessage bound_id1))
             \<longrightarrow> (\<forall>bound_tx4.
               (((transactionOrigin bound_tx4) = (Some bound_i2))
                 \<longrightarrow> ((\<forall>bound_tx22. (((transactionOrigin bound_tx22) = (Some bound_i2)) \<longrightarrow> (bound_tx4 = bound_tx22)))
                   \<and> ((\<exists>bound_c17.
                     (\<exists>bound_c26.
                       (\<exists>bound_c31.
                         (\<exists>bound_r2.
                           (((((((((callOrigin bound_c17) = (Some bound_tx4)) \<and> (bound_c17 \<in> (happensBefore bound_c26)))
                             \<and> ((callOrigin bound_c26) = (Some bound_tx4)))
                             \<and> ((calls bound_c26) = (chat_remove bound_id1)))
                             \<and> (bound_c26 \<in> (happensBefore bound_c31)))
                             \<and> ((callOrigin bound_c31) = (Some bound_tx4)))
                             \<and> ((calls bound_c31) = (message_delete bound_id1)))
                             \<and> (\<forall>bound_c6.
                               (((callOrigin bound_c6) = (Some bound_tx4))
                                 \<longrightarrow> (((bound_c6 = bound_c17) \<or> (bound_c6 = bound_c26)) \<or> (bound_c6 = bound_c31)))))))))
                     \<or> (\<exists>bound_c18.
                       (\<exists>bound_r3.
                         (((callOrigin bound_c18) = (Some bound_tx4))
                           \<and> (\<forall>bound_c7. (((callOrigin bound_c7) = (Some bound_tx4)) \<longrightarrow> (bound_c7 = bound_c18)))))))))))))"
         
assumes before_procedure_invocation_invariant_4:

        "(\<forall>bound_i1.
         (\<forall>bound_id.
           (\<forall>bound_newContent.
             (((invocationOp bound_i1) = (editMessage bound_id bound_newContent))
               \<longrightarrow> (\<forall>bound_tx3.
                 (((transactionOrigin bound_tx3) = (Some bound_i1))
                   \<longrightarrow> ((\<forall>bound_tx21. (((transactionOrigin bound_tx21) = (Some bound_i1)) \<longrightarrow> (bound_tx3 = bound_tx21)))
                     \<and> ((\<exists>bound_c15.
                       (\<exists>bound_c25.
                         (\<exists>bound_r.
                           ((((((callOrigin bound_c15) = (Some bound_tx3)) \<and> (bound_c15 \<in> (happensBefore bound_c25)))
                             \<and> ((callOrigin bound_c25) = (Some bound_tx3)))
                             \<and> ((calls bound_c25) = (message_content_assign bound_id bound_newContent)))
                             \<and> (\<forall>bound_c4.
                               (((callOrigin bound_c4) = (Some bound_tx3)) \<longrightarrow> ((bound_c4 = bound_c15) \<or> (bound_c4 = bound_c25))))))))
                       \<or> (\<exists>bound_c16.
                         (\<exists>bound_r1.
                           (((callOrigin bound_c16) = (Some bound_tx3))
                             \<and> (\<forall>bound_c5. (((callOrigin bound_c5) = (Some bound_tx3)) \<longrightarrow> (bound_c5 = bound_c16))))))))))))))"
         
assumes before_procedure_invocation_invariant_3:

        "(\<forall>bound_i.
         (\<forall>bound_from.
           (\<forall>bound_content1.
             (((invocationOp bound_i) = (sendMessage bound_from bound_content1))
               \<longrightarrow> (\<forall>bound_tx1.
                 (((transactionOrigin bound_tx1) = (Some bound_i))
                   \<longrightarrow> ((\<forall>bound_tx2. (((transactionOrigin bound_tx2) = (Some bound_i)) \<longrightarrow> (bound_tx1 = bound_tx2)))
                     \<and> (\<exists>bound_c14.
                       (\<exists>bound_c24.
                         (\<exists>bound_c3.
                           (\<exists>bound_m4.
                             ((((((((((callOrigin bound_c14) = (Some bound_tx1))
                               \<and> ((calls bound_c14) = (message_author_assign bound_m4 bound_from)))
                               \<and> (bound_c14 \<in> (happensBefore bound_c24)))
                               \<and> ((callOrigin bound_c24) = (Some bound_tx1)))
                               \<and> ((calls bound_c24) = (message_content_assign bound_m4 bound_content1)))
                               \<and> (bound_c24 \<in> (happensBefore bound_c3)))
                               \<and> ((callOrigin bound_c3) = (Some bound_tx1)))
                               \<and> ((calls bound_c3) = (chat_add bound_m4)))
                               \<and> (\<forall>bound_c.
                                 (((callOrigin bound_c) = (Some bound_tx1))
                                   \<longrightarrow> (((bound_c = bound_c14) \<or> (bound_c = bound_c24)) \<or> (bound_c = bound_c3))))))))))))))))"
         
assumes before_procedure_invocation_invariant_2:

        "\<not>(\<exists>bound_write.
         (\<exists>bound_delete.
           (\<exists>bound_m3.
             ((((\<exists>bound_u. ((calls bound_write) = (message_author_assign bound_m3 bound_u)))
               \<or> (\<exists>bound_s1. ((calls bound_write) = (message_content_assign bound_m3 bound_s1))))
               \<and> ((calls bound_delete) = (message_delete bound_m3)))
               \<and> (bound_delete \<in> (happensBefore bound_write))))))"
         
assumes before_procedure_invocation_invariant_1:

        "(\<forall>bound_g.
         (\<forall>bound_m2.
           (\<forall>bound_author.
             (\<forall>bound_content.
               ((((invocationOp bound_g) = (getMessage bound_m2))
                 \<and> ((invocationRes bound_g) = (getMessage_res (found bound_author bound_content))))
                 \<longrightarrow> (\<exists>bound_s. (\<exists>bound_content2. ((invocationOp bound_s) = (sendMessage bound_author bound_content2)))))))))"
         
assumes chat_contains_res_def:

        "(\<forall>bound_m.
         ((chat_contains_res bound_m)
           = (\<exists>bound_c1.
           (((bound_c1 \<in> snapshotAddition) \<and> ((calls bound_c1) = (chat_add bound_m)))
             \<and> (\<forall>bound_c2.
               (((bound_c2 \<in> snapshotAddition) \<and> ((calls bound_c2) = (chat_remove bound_m)))
                 \<longrightarrow> (bound_c2 \<in> (happensBefore bound_c1))))))))"
         
assumes message_exists_res_def:

        "(\<forall>bound_m.
         ((message_exists_res bound_m)
           = (\<exists>bound_c11.
           ((((bound_c11 \<in> snapshotAddition) \<and> (\<exists>bound_args. ((calls bound_c11) = (message_author_assign bound_m bound_args))))
             \<or> ((bound_c11 \<in> snapshotAddition)
               \<and> (\<exists>bound_args1. ((calls bound_c11) = (message_content_assign bound_m bound_args1)))))
             \<and> (\<forall>bound_c21.
               (((bound_c21 \<in> snapshotAddition) \<and> ((calls bound_c21) = (message_delete bound_m)))
                 \<longrightarrow> (bound_c21 \<in> (happensBefore bound_c11))))))))"
         
assumes for_snapshot_additions_def:

        "(for_snapshot_additions = (\<forall>bound_m. ((chat_contains_res bound_m) \<longrightarrow> (message_exists_res bound_m))))"
         
assumes chat_contains_res_def_2:

        "(\<forall>bound_m1.
         ((chat_contains_res1 bound_m1)
           = (\<exists>bound_c12.
           (((bound_c12 \<in> ({} \<union> snapshotAddition)) \<and> ((calls bound_c12) = (chat_add bound_m1)))
             \<and> (\<forall>bound_c22.
               (((bound_c22 \<in> ({} \<union> snapshotAddition)) \<and> ((calls bound_c22) = (chat_remove bound_m1)))
                 \<longrightarrow> (bound_c22 \<in> (happensBefore bound_c12))))))))"
         
assumes message_exists_res_def_2:

        "(\<forall>bound_m1.
         ((message_exists_res1 bound_m1)
           = (\<exists>bound_c13.
           ((((bound_c13 \<in> ({} \<union> snapshotAddition))
             \<and> (\<exists>bound_args2. ((calls bound_c13) = (message_author_assign bound_m1 bound_args2))))
             \<or> ((bound_c13 \<in> ({} \<union> snapshotAddition))
               \<and> (\<exists>bound_args3. ((calls bound_c13) = (message_content_assign bound_m1 bound_args3)))))
             \<and> (\<forall>bound_c23.
               (((bound_c23 \<in> ({} \<union> snapshotAddition)) \<and> ((calls bound_c23) = (message_delete bound_m1)))
                 \<longrightarrow> (bound_c23 \<in> (happensBefore bound_c13))))))))"
         
assumes for_snapshot_additions_with_visibleCalls_def:

        "(for_snapshot_additions_with_visibleCalls
         = (\<forall>bound_m1. ((chat_contains_res1 bound_m1) \<longrightarrow> (message_exists_res1 bound_m1))))"
         
assumes before_procedure_invocation_invariant_0:

        "(for_snapshot_additions \<and> for_snapshot_additions_with_visibleCalls)"
         
assumes i_fresh:

        "((invocationOp currentInvocation) = no_invocation)"
         
assumes old_transactions_unchanged:

        "(\<forall>c5.
         (\<forall>tx4.
           (((((calls c5) = no_call) \<and> ((calls1 c5) \<noteq> no_call)) \<and> ((callOrigin1 c5) = (Some tx4)))
             \<longrightarrow> ((transactionOrigin tx4) = None))))"
         
assumes growth_invocation_res:

        "(\<forall>i5. (((invocationRes i5) \<noteq> NoReturn) \<longrightarrow> ((invocationRes i5) = (invocationRes i5))))"
         
assumes growth_invocation_op:

        "(\<forall>i4.
         ((((invocationOp(currentInvocation := (sendMessage from_init content_init))) i4) \<noteq> no_invocation)
           \<longrightarrow> (((invocationOp(currentInvocation := (sendMessage from_init content_init))) i4)
             = ((invocationOp(currentInvocation := (sendMessage from_init content_init))) i4))))"
         
assumes growth_tx_origin:

        "(\<forall>tx3. (\<not>((transactionOrigin tx3) = None) \<longrightarrow> ((transactionOrigin1 tx3) = (transactionOrigin tx3))))"
         
assumes growth_call_tx:

        "(\<forall>c4. (((calls c4) \<noteq> no_call) \<longrightarrow> ((callOrigin1 c4) = (callOrigin c4))))"
         
assumes growth_happensbefore:

        "(\<forall>c3. (((calls c3) \<noteq> no_call) \<longrightarrow> ((happensBefore1 c3) = (happensBefore c3))))"
         
assumes growth_calls:

        "(\<forall>c2. (((calls c2) \<noteq> no_call) \<longrightarrow> ((calls1 c2) = (calls c2))))"
         
assumes growth_visible_calls:

        "(\<forall>c1. ((c1 \<in> {}) \<longrightarrow> (c1 \<in> {})))"
         
assumes growth_callOrigin:

        "(\<forall>c. (\<forall>tx2. (((callOrigin c) = (Some tx2)) \<longrightarrow> ((callOrigin1 c) = (Some tx2)))))"
         
assumes transaction_begin_snapshot_addition_transaction_consistent:

        "(\<forall>bound_c146.
         (\<forall>bound_c237.
           (((bound_c146 \<in> snapshotAddition1) \<and> (bound_c237 \<in> (happensBefore1 bound_c146))) \<longrightarrow> (bound_c237 \<in> snapshotAddition1))))"
         
assumes transaction_begin_snapshot_addition_transaction_consistent_2:

        "(\<forall>bound_c145.
         (\<forall>bound_c236.
           (((bound_c145 \<in> snapshotAddition1) \<and> ((callOrigin1 bound_c145) = (callOrigin1 bound_c236)))
             \<longrightarrow> (bound_c236 \<in> snapshotAddition1))))"
         
assumes transaction_begin_snapshot_addition_subset_calls:

        "(\<forall>bound_c67. ((bound_c67 \<in> snapshotAddition1) \<longrightarrow> ((calls1 bound_c67) \<noteq> no_call)))"
         
assumes transaction_begin_MessageId_knownIds_are_generated:

        "(\<forall>bound_x6. ((bound_x6 \<in> knownIds_MessageId1) \<longrightarrow> \<not>((generatedIds_MessageId1 bound_x6) = None)))"
         
assumes transaction_begin_message_delete_call_parameter_key_generated:

        "(\<forall>bound_c66.
         (\<forall>bound_key5. (((calls1 bound_c66) = (message_delete bound_key5)) \<longrightarrow> \<not>((generatedIds_MessageId1 bound_key5) = None))))"
         
assumes transaction_begin_message_content_assign_call_parameter_key_generated:

        "(\<forall>bound_c65.
         (\<forall>bound_key4.
           (\<forall>bound_value3.
             (((calls1 bound_c65) = (message_content_assign bound_key4 bound_value3))
               \<longrightarrow> \<not>((generatedIds_MessageId1 bound_key4) = None)))))"
         
assumes transaction_begin_message_author_assign_call_parameter_key_generated:

        "(\<forall>bound_c64.
         (\<forall>bound_key3.
           (\<forall>bound_value2.
             (((calls1 bound_c64) = (message_author_assign bound_key3 bound_value2))
               \<longrightarrow> \<not>((generatedIds_MessageId1 bound_key3) = None)))))"
         
assumes transaction_begin_chat_remove_call_parameter_elem_generated:

        "(\<forall>bound_c63.
         (\<forall>bound_elem3. (((calls1 bound_c63) = (chat_remove bound_elem3)) \<longrightarrow> \<not>((generatedIds_MessageId1 bound_elem3) = None))))"
         
assumes transaction_begin_chat_add_call_parameter_elem_generated:

        "(\<forall>bound_c62.
         (\<forall>bound_elem2. (((calls1 bound_c62) = (chat_add bound_elem2)) \<longrightarrow> \<not>((generatedIds_MessageId1 bound_elem2) = None))))"
         
assumes transaction_begin_sendMessage_result_known:

        "(\<forall>bound_i23.
         (\<forall>bound_result1.
           (((invocationRes bound_i23) = (sendMessage_res bound_result1)) \<longrightarrow> (bound_result1 \<in> knownIds_MessageId1))))"
         
assumes transaction_begin_getMessage_parameter_m_known:

        "(\<forall>bound_i22.
         (\<forall>bound_m15.
           ((((invocationOp(currentInvocation := (sendMessage from_init content_init))) bound_i22) = (getMessage bound_m15))
             \<longrightarrow> (bound_m15 \<in> knownIds_MessageId1))))"
         
assumes transaction_begin_deleteMessage_parameter_message_id_known:

        "(\<forall>bound_i21.
         (\<forall>bound_message_id1.
           ((((invocationOp(currentInvocation := (sendMessage from_init content_init))) bound_i21)
             = (deleteMessage bound_message_id1))
             \<longrightarrow> (bound_message_id1 \<in> knownIds_MessageId1))))"
         
assumes transaction_begin_editMessage_parameter_id_known:

        "(\<forall>bound_i20.
         (\<forall>bound_id7.
           (\<forall>bound_newContent3.
             ((((invocationOp(currentInvocation := (sendMessage from_init content_init))) bound_i20)
               = (editMessage bound_id7 bound_newContent3))
               \<longrightarrow> (bound_id7 \<in> knownIds_MessageId1)))))"
         
assumes transaction_begin_WF_transactionOrigin_exists:

        "(\<forall>bound_tx32.
         (\<forall>bound_i19.
           (((transactionOrigin1 bound_tx32) = (Some bound_i19))
             \<longrightarrow> (((invocationOp(currentInvocation := (sendMessage from_init content_init))) bound_i19) \<noteq> no_invocation))))"
         
assumes transaction_begin_WF_callOrigin_exists:

        "(\<forall>bound_ca3.
         (\<forall>bound_tx31. (((callOrigin1 bound_ca3) = (Some bound_tx31)) \<longrightarrow> \<not>((transactionOrigin1 bound_tx31) = None))))"
         
assumes transaction_begin_WF_no_call_implies_not_in_happensBefore:

        "(\<forall>bound_ca2. (\<forall>bound_cb1. (((callOrigin1 bound_ca2) = None) \<longrightarrow> \<not>(bound_ca2 \<in> (happensBefore1 bound_cb1)))))"
         
assumes transaction_begin_WF_no_call_implies_no_happensBefore:

        "(\<forall>bound_c61. (((callOrigin1 bound_c61) = None) \<longrightarrow> ((happensBefore1 bound_c61) = {})))"
         
assumes transaction_begin_WF_transactionOrigin_callOrigin:

        "(\<forall>bound_tx30. (((transactionOrigin1 bound_tx30) = None) \<longrightarrow> (\<forall>bound_c60. ((callOrigin1 bound_c60) \<noteq> (Some bound_tx30)))))"
         
assumes transaction_begin_WF_callOrigin:

        "(\<forall>bound_c59. (((callOrigin1 bound_c59) = None) = ((calls1 bound_c59) = no_call)))"
         
assumes transaction_begin_WF_invocationCalls:

        "(\<forall>bound_i18.
         (\<forall>bound_c58.
           ((bound_c58 \<in> (invocationCalls1 bound_i18))
             = (\<exists>bound_tx20.
             (((callOrigin1 bound_c58) = (Some bound_tx20)) \<and> ((transactionOrigin1 bound_tx20) = (Some bound_i18)))))))"
         
assumes transaction_begin_no_invocation_implies_no_result:

        "(\<forall>bound_x12.
         (\<forall>bound_x21.
           (\<forall>bound_y12.
             (\<forall>bound_y22.
               ((((((callOrigin1 bound_x12) = (callOrigin1 bound_x21)) \<and> ((callOrigin1 bound_y12) = (callOrigin1 bound_y22)))
                 \<and> \<not>((callOrigin1 bound_x12) = (callOrigin1 bound_y12)))
                 \<and> (bound_x21 \<in> (happensBefore1 bound_y12)))
                 \<longrightarrow> (bound_x21 \<in> (happensBefore1 bound_y22)))))))"
         
assumes transaction_begin_no_invocation_implies_no_result_2:

        "(\<forall>bound_i17.
         ((((invocationOp(currentInvocation := (sendMessage from_init content_init))) bound_i17) = no_invocation)
           \<longrightarrow> ((invocationRes bound_i17) = NoReturn)))"
         
assumes transaction_begin_happensBefore_antisym:

        "(\<forall>bound_x5.
         (\<forall>bound_y4.
           (((bound_x5 \<in> (happensBefore1 bound_y4)) \<and> (bound_y4 \<in> (happensBefore1 bound_x5))) \<longrightarrow> (bound_x5 = bound_y4))))"
         
assumes transaction_begin_happensBefore_trans:

        "(\<forall>bound_x4.
         (\<forall>bound_y3.
           (\<forall>bound_z1.
             (((bound_x4 \<in> (happensBefore1 bound_y3)) \<and> (bound_y3 \<in> (happensBefore1 bound_z1)))
               \<longrightarrow> (bound_x4 \<in> (happensBefore1 bound_y3))))))"
         
assumes transaction_begin_happensBefore_reflex:

        "(\<forall>bound_c57. (((calls1 bound_c57) \<noteq> no_call) \<longrightarrow> (bound_c57 \<in> (happensBefore1 bound_c57))))"
         
assumes transaction_begin_visibleCalls_causally_consistent:

        "(\<forall>bound_c144. (\<forall>bound_c235. (((bound_c235 \<in> vis) \<and> (bound_c144 \<in> (happensBefore1 bound_c235))) \<longrightarrow> (bound_c144 \<in> vis))))"
         
assumes transaction_begin_visibleCalls_transaction_consistent1:

        "(\<forall>bound_c143.
         (\<forall>bound_c234.
           ((((bound_c143 \<in> vis) \<and> ((callOrigin1 bound_c143) = (callOrigin1 bound_c234))) \<and> ((calls1 bound_c234) \<noteq> no_call))
             \<longrightarrow> (bound_c234 \<in> vis))))"
         
assumes transaction_begin_visibleCalls_exist:

        "(\<forall>bound_c56. ((bound_c56 \<in> vis) \<longrightarrow> ((calls1 bound_c56) \<noteq> no_call)))"
         
assumes transaction_begin_invocation_sequential:

        "(\<forall>bound_c142.
         (\<forall>bound_tx19.
           (\<forall>bound_i16.
             (\<forall>bound_c233.
               (\<forall>bound_tx213.
                 ((((((callOrigin1 bound_c142) = (Some bound_tx19)) \<and> ((transactionOrigin1 bound_tx19) = (Some bound_i16)))
                   \<and> ((callOrigin1 bound_c233) = (Some bound_tx213)))
                   \<and> ((transactionOrigin1 bound_tx213) = (Some bound_i16)))
                   \<longrightarrow> ((bound_c142 \<in> (happensBefore1 bound_c233)) \<or> (bound_c233 \<in> (happensBefore1 bound_c142)))))))))"
         
assumes transaction_begin_happensBefore_exists_r:

        "(\<forall>bound_c141. (\<forall>bound_c232. (((calls1 bound_c141) = no_call) \<longrightarrow> \<not>(bound_c141 \<in> (happensBefore1 bound_c232)))))"
         
assumes transaction_begin_happensBefore_exists_l:

        "(\<forall>bound_c140. (\<forall>bound_c231. (((calls1 bound_c140) = no_call) \<longrightarrow> \<not>(bound_c140 \<in> (happensBefore1 bound_c231)))))"
         
assumes at_transaction_begin_invariant_6:

        "(\<forall>bound_i15.
         (\<forall>bound_id6.
           ((((invocationOp(currentInvocation := (sendMessage from_init content_init))) bound_i15) = (getMessage bound_id6))
             \<longrightarrow> (\<forall>bound_tx18.
               (((transactionOrigin1 bound_tx18) = (Some bound_i15))
                 \<longrightarrow> ((\<forall>bound_tx212. (((transactionOrigin1 bound_tx212) = (Some bound_i15)) \<longrightarrow> (bound_tx18 = bound_tx212)))
                   \<and> ((\<exists>bound_c138.
                     (\<exists>bound_c230.
                       (\<exists>bound_c315.
                         (\<exists>bound_r16.
                           (((((((callOrigin1 bound_c138) = (Some bound_tx18)) \<and> (bound_c138 \<in> (happensBefore1 bound_c230)))
                             \<and> ((callOrigin1 bound_c230) = (Some bound_tx18)))
                             \<and> (bound_c230 \<in> (happensBefore1 bound_c315)))
                             \<and> ((callOrigin1 bound_c315) = (Some bound_tx18)))
                             \<and> (\<forall>bound_c54.
                               (((callOrigin1 bound_c54) = (Some bound_tx18))
                                 \<longrightarrow> (((bound_c54 = bound_c138) \<or> (bound_c54 = bound_c230)) \<or> (bound_c54 = bound_c315)))))))))
                     \<or> (\<exists>bound_c139.
                       (\<exists>bound_r17.
                         (((callOrigin1 bound_c139) = (Some bound_tx18))
                           \<and> (\<forall>bound_c55. (((callOrigin1 bound_c55) = (Some bound_tx18)) \<longrightarrow> (bound_c55 = bound_c139)))))))))))))"
         
assumes at_transaction_begin_invariant_5:

        "(\<forall>bound_i14.
         (\<forall>bound_id5.
           ((((invocationOp(currentInvocation := (sendMessage from_init content_init))) bound_i14) = (deleteMessage bound_id5))
             \<longrightarrow> (\<forall>bound_tx17.
               (((transactionOrigin1 bound_tx17) = (Some bound_i14))
                 \<longrightarrow> ((\<forall>bound_tx211. (((transactionOrigin1 bound_tx211) = (Some bound_i14)) \<longrightarrow> (bound_tx17 = bound_tx211)))
                   \<and> ((\<exists>bound_c136.
                     (\<exists>bound_c229.
                       (\<exists>bound_c314.
                         (\<exists>bound_r14.
                           (((((((((callOrigin1 bound_c136) = (Some bound_tx17)) \<and> (bound_c136 \<in> (happensBefore1 bound_c229)))
                             \<and> ((callOrigin1 bound_c229) = (Some bound_tx17)))
                             \<and> ((calls1 bound_c229) = (chat_remove bound_id5)))
                             \<and> (bound_c229 \<in> (happensBefore1 bound_c314)))
                             \<and> ((callOrigin1 bound_c314) = (Some bound_tx17)))
                             \<and> ((calls1 bound_c314) = (message_delete bound_id5)))
                             \<and> (\<forall>bound_c52.
                               (((callOrigin1 bound_c52) = (Some bound_tx17))
                                 \<longrightarrow> (((bound_c52 = bound_c136) \<or> (bound_c52 = bound_c229)) \<or> (bound_c52 = bound_c314)))))))))
                     \<or> (\<exists>bound_c137.
                       (\<exists>bound_r15.
                         (((callOrigin1 bound_c137) = (Some bound_tx17))
                           \<and> (\<forall>bound_c53. (((callOrigin1 bound_c53) = (Some bound_tx17)) \<longrightarrow> (bound_c53 = bound_c137)))))))))))))"
         
assumes at_transaction_begin_invariant_4:

        "(\<forall>bound_i13.
         (\<forall>bound_id4.
           (\<forall>bound_newContent2.
             ((((invocationOp(currentInvocation := (sendMessage from_init content_init))) bound_i13)
               = (editMessage bound_id4 bound_newContent2))
               \<longrightarrow> (\<forall>bound_tx16.
                 (((transactionOrigin1 bound_tx16) = (Some bound_i13))
                   \<longrightarrow> ((\<forall>bound_tx210. (((transactionOrigin1 bound_tx210) = (Some bound_i13)) \<longrightarrow> (bound_tx16 = bound_tx210)))
                     \<and> ((\<exists>bound_c134.
                       (\<exists>bound_c228.
                         (\<exists>bound_r12.
                           ((((((callOrigin1 bound_c134) = (Some bound_tx16)) \<and> (bound_c134 \<in> (happensBefore1 bound_c228)))
                             \<and> ((callOrigin1 bound_c228) = (Some bound_tx16)))
                             \<and> ((calls1 bound_c228) = (message_content_assign bound_id4 bound_newContent2)))
                             \<and> (\<forall>bound_c50.
                               (((callOrigin1 bound_c50) = (Some bound_tx16))
                                 \<longrightarrow> ((bound_c50 = bound_c134) \<or> (bound_c50 = bound_c228))))))))
                       \<or> (\<exists>bound_c135.
                         (\<exists>bound_r13.
                           (((callOrigin1 bound_c135) = (Some bound_tx16))
                             \<and> (\<forall>bound_c51. (((callOrigin1 bound_c51) = (Some bound_tx16)) \<longrightarrow> (bound_c51 = bound_c135))))))))))))))"
         
assumes at_transaction_begin_invariant_3:

        "(\<forall>bound_i12.
         (\<forall>bound_from1.
           (\<forall>bound_content4.
             ((((invocationOp(currentInvocation := (sendMessage from_init content_init))) bound_i12)
               = (sendMessage bound_from1 bound_content4))
               \<longrightarrow> (\<forall>bound_tx15.
                 (((transactionOrigin1 bound_tx15) = (Some bound_i12))
                   \<longrightarrow> ((\<forall>bound_tx29. (((transactionOrigin1 bound_tx29) = (Some bound_i12)) \<longrightarrow> (bound_tx15 = bound_tx29)))
                     \<and> (\<exists>bound_c133.
                       (\<exists>bound_c227.
                         (\<exists>bound_c313.
                           (\<exists>bound_m14.
                             ((((((((((callOrigin1 bound_c133) = (Some bound_tx15))
                               \<and> ((calls1 bound_c133) = (message_author_assign bound_m14 bound_from1)))
                               \<and> (bound_c133 \<in> (happensBefore1 bound_c227)))
                               \<and> ((callOrigin1 bound_c227) = (Some bound_tx15)))
                               \<and> ((calls1 bound_c227) = (message_content_assign bound_m14 bound_content4)))
                               \<and> (bound_c227 \<in> (happensBefore1 bound_c313)))
                               \<and> ((callOrigin1 bound_c313) = (Some bound_tx15)))
                               \<and> ((calls1 bound_c313) = (chat_add bound_m14)))
                               \<and> (\<forall>bound_c49.
                                 (((callOrigin1 bound_c49) = (Some bound_tx15))
                                   \<longrightarrow> (((bound_c49 = bound_c133) \<or> (bound_c49 = bound_c227)) \<or> (bound_c49 = bound_c313))))))))))))))))"
         
assumes at_transaction_begin_invariant_2:

        "\<not>(\<exists>bound_write2.
         (\<exists>bound_delete2.
           (\<exists>bound_m13.
             ((((\<exists>bound_u2. ((calls1 bound_write2) = (message_author_assign bound_m13 bound_u2)))
               \<or> (\<exists>bound_s5. ((calls1 bound_write2) = (message_content_assign bound_m13 bound_s5))))
               \<and> ((calls1 bound_delete2) = (message_delete bound_m13)))
               \<and> (bound_delete2 \<in> (happensBefore1 bound_write2))))))"
         
assumes at_transaction_begin_invariant_1:

        "(\<forall>bound_g1.
         (\<forall>bound_m12.
           (\<forall>bound_author1.
             (\<forall>bound_content3.
               (((((invocationOp(currentInvocation := (sendMessage from_init content_init))) bound_g1) = (getMessage bound_m12))
                 \<and> ((invocationRes bound_g1) = (getMessage_res (found bound_author1 bound_content3))))
                 \<longrightarrow> (\<exists>bound_s4.
                   (\<exists>bound_content22.
                     (((invocationOp(currentInvocation := (sendMessage from_init content_init))) bound_s4)
                       = (sendMessage bound_author1 bound_content22)))))))))"
         
assumes chat_contains_res_def_3:

        "(\<forall>bound_m10.
         ((chat_contains_res2 bound_m10)
           = (\<exists>bound_c129.
           (((bound_c129 \<in> snapshotAddition1) \<and> ((calls1 bound_c129) = (chat_add bound_m10)))
             \<and> (\<forall>bound_c223.
               (((bound_c223 \<in> snapshotAddition1) \<and> ((calls1 bound_c223) = (chat_remove bound_m10)))
                 \<longrightarrow> (bound_c223 \<in> (happensBefore1 bound_c129))))))))"
         
assumes message_exists_res_def_3:

        "(\<forall>bound_m10.
         ((message_exists_res2 bound_m10)
           = (\<exists>bound_c130.
           ((((bound_c130 \<in> snapshotAddition1)
             \<and> (\<exists>bound_args8. ((calls1 bound_c130) = (message_author_assign bound_m10 bound_args8))))
             \<or> ((bound_c130 \<in> snapshotAddition1)
               \<and> (\<exists>bound_args9. ((calls1 bound_c130) = (message_content_assign bound_m10 bound_args9)))))
             \<and> (\<forall>bound_c224.
               (((bound_c224 \<in> snapshotAddition1) \<and> ((calls1 bound_c224) = (message_delete bound_m10)))
                 \<longrightarrow> (bound_c224 \<in> (happensBefore1 bound_c130))))))))"
         
assumes for_snapshot_additions_def_2:

        "(for_snapshot_additions1 = (\<forall>bound_m10. ((chat_contains_res2 bound_m10) \<longrightarrow> (message_exists_res2 bound_m10))))"
         
assumes chat_contains_res_def_4:

        "(\<forall>bound_m11.
         ((chat_contains_res3 bound_m11)
           = (\<exists>bound_c131.
           (((bound_c131 \<in> (vis \<union> snapshotAddition1)) \<and> ((calls1 bound_c131) = (chat_add bound_m11)))
             \<and> (\<forall>bound_c225.
               (((bound_c225 \<in> (vis \<union> snapshotAddition1)) \<and> ((calls1 bound_c225) = (chat_remove bound_m11)))
                 \<longrightarrow> (bound_c225 \<in> (happensBefore1 bound_c131))))))))"
         
assumes message_exists_res_def_4:

        "(\<forall>bound_m11.
         ((message_exists_res3 bound_m11)
           = (\<exists>bound_c132.
           ((((bound_c132 \<in> (vis \<union> snapshotAddition1))
             \<and> (\<exists>bound_args10. ((calls1 bound_c132) = (message_author_assign bound_m11 bound_args10))))
             \<or> ((bound_c132 \<in> (vis \<union> snapshotAddition1))
               \<and> (\<exists>bound_args11. ((calls1 bound_c132) = (message_content_assign bound_m11 bound_args11)))))
             \<and> (\<forall>bound_c226.
               (((bound_c226 \<in> (vis \<union> snapshotAddition1)) \<and> ((calls1 bound_c226) = (message_delete bound_m11)))
                 \<longrightarrow> (bound_c226 \<in> (happensBefore1 bound_c132))))))))"
         
assumes for_snapshot_additions_with_visibleCalls_def_2:

        "(for_snapshot_additions_with_visibleCalls1
         = (\<forall>bound_m11. ((chat_contains_res3 bound_m11) \<longrightarrow> (message_exists_res3 bound_m11))))"
         
assumes at_transaction_begin_invariant_0:

        "(for_snapshot_additions1 \<and> for_snapshot_additions_with_visibleCalls1)"
         
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
         
assumes m_new_id_fresh:

        "((generatedIds_MessageId1 m2) = None)"
         
assumes c0_freshB:

        "((calls1 c0) = no_call)"
         
assumes c0_freshA:

        "distinct [c0]"
         
assumes calls_def:

        "(calls2 = (calls1(c0 := (message_author_assign m2 from_init))))"
         
assumes c11_freshB:

        "((calls2 c11) = no_call)"
         
assumes c11_freshA:

        "distinct [c11 , c0]"
         
assumes calls_def_2:

        "(calls3 = (calls2(c11 := (message_content_assign m2 content_init))))"
         
assumes c21_freshB:

        "((calls3 c21) = no_call)"
         
assumes c21_freshA:

        "distinct [c21 , c0 , c11]"
         
assumes callOrigin_def:

        "(callOrigin2 = (callOrigin1(c0 := (Some tx1))))"
         
assumes callOrigin_def_2:

        "(callOrigin3 = (callOrigin2(c11 := (Some tx1))))"
         
assumes callOrigin_def_3:

        "(callOrigin4 = (callOrigin3(c21 := (Some tx1))))"
         
assumes calls_def_3:

        "(calls4 = (calls3(c21 := (chat_add m2))))"
         
assumes vis_def:

        "(vis1 = (vis \<union> {c0}))"
         
assumes vis_def_2:

        "(vis2 = (vis1 \<union> {c11}))"
         
assumes happensBefore_def:

        "(happensBefore2 = (happensBefore1(c0 := (vis \<union> {c0}))))"
         
assumes happensBefore_def_2:

        "(happensBefore3 = (happensBefore2(c11 := (vis1 \<union> {c11}))))"
         
assumes happensBefore_def_3:

        "(happensBefore4 = (happensBefore3(c21 := (vis2 \<union> {c21}))))"
         
assumes invariant_not_violated:

        "\<not>((((invocationOp(currentInvocation := (sendMessage from_init content_init))) i6) = (sendMessage from2 content5))
         \<longrightarrow> (\<forall>bound_tx37.
           ((((transactionOrigin1(tx1 := (Some currentInvocation))) bound_tx37) = (Some i6))
             \<longrightarrow> ((\<forall>bound_tx215.
               ((((transactionOrigin1(tx1 := (Some currentInvocation))) bound_tx215) = (Some i6)) \<longrightarrow> (bound_tx37 = bound_tx215)))
               \<and> (\<exists>bound_c158.
                 (\<exists>bound_c249.
                   (\<exists>bound_c316.
                     (\<exists>bound_m20.
                       ((((((((((callOrigin4 bound_c158) = (Some bound_tx37))
                         \<and> ((calls4 bound_c158) = (message_author_assign bound_m20 from2)))
                         \<and> (bound_c158 \<in> (happensBefore4 bound_c249)))
                         \<and> ((callOrigin4 bound_c249) = (Some bound_tx37)))
                         \<and> ((calls4 bound_c249) = (message_content_assign bound_m20 content5)))
                         \<and> (bound_c249 \<in> (happensBefore4 bound_c316)))
                         \<and> ((callOrigin4 bound_c316) = (Some bound_tx37)))
                         \<and> ((calls4 bound_c316) = (chat_add bound_m20)))
                         \<and> (\<forall>bound_c80.
                           (((callOrigin4 bound_c80) = (Some bound_tx37))
                             \<longrightarrow> (((bound_c80 = bound_c158) \<or> (bound_c80 = bound_c249)) \<or> (bound_c80 = bound_c316)))))))))))))"
      shows False
  using invariant_not_violated proof (rule notE, intro impI)
  assume a1: "\<not> ((invocationOp(currentInvocation := sendMessage from_init content_init)) i6 = sendMessage from2 content5 \<longrightarrow>
        (\<forall>bound_tx37.
            (transactionOrigin1(tx1 \<mapsto> currentInvocation)) bound_tx37 = Some i6 \<longrightarrow>
            (\<forall>bound_tx215. (transactionOrigin1(tx1 \<mapsto> currentInvocation)) bound_tx215 = Some i6 \<longrightarrow> bound_tx37 = bound_tx215) \<and>
            (\<exists>bound_c158 bound_c249 bound_c316 bound_m20.
                (((((((callOrigin4 bound_c158 = Some bound_tx37 \<and> calls4 bound_c158 = message_author_assign bound_m20 from2) \<and>
                      bound_c158 \<in> happensBefore4 bound_c249) \<and>
                     callOrigin4 bound_c249 = Some bound_tx37) \<and>
                    calls4 bound_c249 = message_content_assign bound_m20 content5) \<and>
                   bound_c249 \<in> happensBefore4 bound_c316) \<and>
                  callOrigin4 bound_c316 = Some bound_tx37) \<and>
                 calls4 bound_c316 = chat_add bound_m20) \<and>
                (\<forall>bound_c80.
                    callOrigin4 bound_c80 = Some bound_tx37 \<longrightarrow>
                    (bound_c80 = bound_c158 \<or> bound_c80 = bound_c249) \<or> bound_c80 = bound_c316))))"
and a2: "    (invocationOp(currentInvocation := sendMessage from_init content_init)) i6 = sendMessage from2 content5"

  show " \<forall>bound_tx37.
           (transactionOrigin1(tx1 \<mapsto> currentInvocation)) bound_tx37 = Some i6 \<longrightarrow>
           (\<forall>bound_tx215. (transactionOrigin1(tx1 \<mapsto> currentInvocation)) bound_tx215 = Some i6 \<longrightarrow> bound_tx37 = bound_tx215) \<and>
           (\<exists>bound_c158 bound_c249 bound_c316 bound_m20.
               (((((((callOrigin4 bound_c158 = Some bound_tx37 \<and> calls4 bound_c158 = message_author_assign bound_m20 from2) \<and>
                     bound_c158 \<in> happensBefore4 bound_c249) \<and>
                    callOrigin4 bound_c249 = Some bound_tx37) \<and>
                   calls4 bound_c249 = message_content_assign bound_m20 content5) \<and>
                  bound_c249 \<in> happensBefore4 bound_c316) \<and>
                 callOrigin4 bound_c316 = Some bound_tx37) \<and>
                calls4 bound_c316 = chat_add bound_m20) \<and>
               (\<forall>bound_c80.
                   callOrigin4 bound_c80 = Some bound_tx37 \<longrightarrow> (bound_c80 = bound_c158 \<or> bound_c80 = bound_c249) \<or> bound_c80 = bound_c316))"
  proof (cases "i6 = currentInvocation")
    case False

    
    
    show ?thesis 
    proof (intro allI impI conjI)
      fix bound_tx37
      assume aa: "(transactionOrigin1(tx1 \<mapsto> currentInvocation)) bound_tx37 = Some i6"

      from aa 
      have tx_orig: "transactionOrigin1 bound_tx37 = Some i6"
        by (metis False map_upd_Some_unfold)


      thm at_transaction_begin_invariant_3[rule_format, OF a2, OF tx_orig]

      show "bound_tx37 = bound_tx215" if "(transactionOrigin1(tx1 \<mapsto> currentInvocation)) bound_tx215 = Some i6"  for bound_tx215
        using that False at_transaction_begin_invariant_3[rule_format, OF a2, OF tx_orig]
        by (auto split: if_splits)

      show "\<And>bound_tx37.
       (transactionOrigin1(tx1 \<mapsto> currentInvocation)) bound_tx37 = Some i6 \<Longrightarrow>
       \<exists>bound_c158 bound_c249 bound_c316 bound_m20.
          (((((((callOrigin4 bound_c158 = Some bound_tx37 \<and> calls4 bound_c158 = message_author_assign bound_m20 from2) \<and>
                bound_c158 \<in> happensBefore4 bound_c249) \<and>
               callOrigin4 bound_c249 = Some bound_tx37) \<and>
              calls4 bound_c249 = message_content_assign bound_m20 content5) \<and>
             bound_c249 \<in> happensBefore4 bound_c316) \<and>
            callOrigin4 bound_c316 = Some bound_tx37) \<and>
           calls4 bound_c316 = chat_add bound_m20) \<and>
          (\<forall>bound_c80.
              callOrigin4 bound_c80 = Some bound_tx37 \<longrightarrow> (bound_c80 = bound_c158 \<or> bound_c80 = bound_c249) \<or> bound_c80 = bound_c316)"
        using at_transaction_begin_invariant_3[rule_format, OF a2, OF tx_orig]
      proof - (*sledgehammer generated *)
        fix bound_tx37a :: TxId
        assume a1: "(transactionOrigin1(tx1 \<mapsto> currentInvocation)) bound_tx37a = Some i6"
        obtain cc :: CallId and mm :: MessageId and cca :: CallId and ccb :: CallId where
          f2: "callOrigin1 cc = Some bound_tx37 \<and> calls1 cc = message_author_assign mm from2 \<and> cc \<in> happensBefore1 cca \<and> callOrigin1 cca = Some bound_tx37 \<and> calls1 cca = message_content_assign mm content5 \<and> cca \<in> happensBefore1 ccb \<and> callOrigin1 ccb = Some bound_tx37 \<and> calls1 ccb = chat_add mm \<and> (\<forall>c. callOrigin1 c \<noteq> Some bound_tx37 \<or> c = cc \<or> c = cca \<or> c = ccb)"
          using \<open>(\<forall>bound_tx29. transactionOrigin1 bound_tx29 = Some i6 \<longrightarrow> bound_tx37 = bound_tx29) \<and> (\<exists>bound_c133 bound_c227 bound_c313 bound_m14. (((((((callOrigin1 bound_c133 = Some bound_tx37 \<and> calls1 bound_c133 = message_author_assign bound_m14 from2) \<and> bound_c133 \<in> happensBefore1 bound_c227) \<and> callOrigin1 bound_c227 = Some bound_tx37) \<and> calls1 bound_c227 = message_content_assign bound_m14 content5) \<and> bound_c227 \<in> happensBefore1 bound_c313) \<and> callOrigin1 bound_c313 = Some bound_tx37) \<and> calls1 bound_c313 = chat_add bound_m14) \<and> (\<forall>bound_c49. callOrigin1 bound_c49 = Some bound_tx37 \<longrightarrow> (bound_c49 = bound_c133 \<or> bound_c49 = bound_c227) \<or> bound_c49 = bound_c313))\<close> by auto
        then have f3: "callOrigin1 cc \<noteq> None"
          by simp
        have f4: "\<forall>c ca cs. distinct ((c::CallId) # ca # cs) = (c \<noteq> ca \<and> distinct (c # cs) \<and> distinct (ca # cs))"
          using distinct_length_2_or_more by blast
        then have f5: "c21 \<noteq> c0 \<and> distinct [c21, c11] \<and> distinct [c0, c11]"
          using c21_freshA by presburger
        then have f6: "c21 \<noteq> c11 \<and> distinct [c21] \<and> distinct [c11]"
          using f4 by metis
        have f7: "\<forall>c ca f cb cc. (c::CallId) = ca \<or> f(c := cb::callInfo, ca := cc) = f(ca := cc, c := cb)"
          by (meson fun_upd_twist)
        have f8: "cca \<noteq> c0"
          using f2 c0_freshB by force
        then have f9: "calls1 (c0 := message_author_assign m2 from_init, cca := message_content_assign mm content5) = calls1(c0 := message_author_assign m2 from_init)"
          using f7 f2 by (metis (no_types) fun_upd_triv)
        have f10: "\<forall>c ca f cb. (c::CallId) = ca \<or> (f(ca := cb::callInfo)) c = f c"
          by simp
        then have f11: "c21 \<noteq> cca"
          using f9 f6 f3 f2 by (metis (no_types) c21_freshB calls_def calls_def_2 fun_upd_same transaction_begin_WF_callOrigin)
        have f12: "c11 \<noteq> cca"
          using f9 f3 f2 by (metis (no_types) c11_freshB calls_def fun_upd_same transaction_begin_WF_callOrigin)
        then have f13: "cc \<in> happensBefore4 cca"
          using f11 f8 f2 by (simp add: happensBefore_def happensBefore_def_2 happensBefore_def_3)
        have f14: "callOrigin4 cca = Some bound_tx37"
          using f12 f11 f8 f2 by (simp add: callOrigin_def callOrigin_def_2 callOrigin_def_3)
        have f15: "bound_tx37 = bound_tx37a"
          using a1 \<open>\<And>bound_tx215. (transactionOrigin1(tx1 \<mapsto> currentInvocation)) bound_tx215 = Some i6 \<Longrightarrow> bound_tx37 = bound_tx215\<close> by presburger
        have f16: "calls4 cca = message_content_assign mm content5"
          using f12 f11 f9 f7 by (metis (no_types) calls_def calls_def_2 calls_def_3 fun_upd_same)
        have f17: "c0 \<noteq> c11 \<and> distinct [c0] \<and> distinct [c11]"
          using f5 by auto
        have f18: "ccb \<noteq> c0"
          using f2 c0_freshB by force
        have f19: "c11 \<noteq> ccb"
          using f17 f2 c11_freshB calls_def by force
        then have f20: "c21 \<noteq> ccb"
          using f18 f7 f2 c21_freshB calls_def calls_def_2 by auto
        then have f21: "cca \<in> happensBefore4 ccb"
          using f19 f18 f2 by (simp add: happensBefore_def happensBefore_def_2 happensBefore_def_3)
        have f22: "callOrigin4 ccb = Some bound_tx37"
          using f20 f19 f18 f2 by (simp add: callOrigin_def callOrigin_def_2 callOrigin_def_3)
        have f23: "calls4 ccb = chat_add mm"
          using f20 f19 f18 f2 by (simp add: calls_def calls_def_2 calls_def_3)
        have "callOrigin1 cc \<noteq> Some tx1"
          using transaction_begin_WF_callOrigin_exists tx1_fresh by blast
        then have f24: "callOrigin4 (v4_0 ccb cca cc) \<noteq> Some bound_tx37a \<or> v4_0 ccb cca cc = cc \<or> v4_0 ccb cca cc = cca \<or> v4_0 ccb cca cc = ccb"
          using f15 f2 by (simp add: callOrigin_def callOrigin_def_2 callOrigin_def_3)
        have f25: "c0 \<noteq> cc \<longrightarrow> calls4 cc = message_author_assign mm from2"
          using f10 f3 f2 by (metis (no_types) c11_freshB c21_freshB calls_def calls_def_2 calls_def_3 transaction_begin_WF_callOrigin)
        have f26: "c0 \<noteq> cc \<longrightarrow> callOrigin4 cc = Some bound_tx37"
          using f10 f2 c11_freshB c21_freshB callOrigin_def callOrigin_def_2 callOrigin_def_3 calls_def calls_def_2 by auto
        have f27: "c0 \<noteq> cc"
          using f3 c0_freshB transaction_begin_WF_callOrigin by auto
        obtain ccc :: "CallId \<Rightarrow> CallId \<Rightarrow> CallId \<Rightarrow> CallId" where
          "\<forall>x1 x2 x3. (\<exists>v4. callOrigin4 v4 = Some bound_tx37a \<and> v4 \<noteq> x3 \<and> v4 \<noteq> x2 \<and> v4 \<noteq> x1) = (callOrigin4 (ccc x1 x2 x3) = Some bound_tx37a \<and> ccc x1 x2 x3 \<noteq> x3 \<and> ccc x1 x2 x3 \<noteq> x2 \<and> ccc x1 x2 x3 \<noteq> x1)"
          by moura
        then show "\<exists>c ca cb m. (((((((callOrigin4 c = Some bound_tx37a \<and> calls4 c = message_author_assign m from2) \<and> c \<in> happensBefore4 ca) \<and> callOrigin4 ca = Some bound_tx37a) \<and> calls4 ca = message_content_assign m content5) \<and> ca \<in> happensBefore4 cb) \<and> callOrigin4 cb = Some bound_tx37a) \<and> calls4 cb = chat_add m) \<and> (\<forall>cc. callOrigin4 cc = Some bound_tx37a \<longrightarrow> (cc = c \<or> cc = ca) \<or> cc = cb)"
          using f27 f26 f25 f24 f23 f22 f21 f16 f15 f14 f13
        proof -
          have f1: "callOrigin4 (v4_0a ccb cca cc) \<noteq> Some bound_tx37a \<or> v4_0a ccb cca cc = cc \<or> v4_0a ccb cca cc = cca \<or> v4_0a ccb cca cc = ccb"
            using \<open>bound_tx37 = bound_tx37a\<close> \<open>callOrigin1 cc \<noteq> Some tx1\<close> callOrigin_def callOrigin_def_2 callOrigin_def_3 f2 by auto
          obtain ccd :: "CallId \<Rightarrow> CallId \<Rightarrow> CallId \<Rightarrow> CallId" where
            "\<forall>x1 x2 x3. (\<exists>v4. callOrigin4 v4 = Some bound_tx37a \<and> v4 \<noteq> x3 \<and> v4 \<noteq> x2 \<and> v4 \<noteq> x1) = (callOrigin4 (ccd x1 x2 x3) = Some bound_tx37a \<and> ccd x1 x2 x3 \<noteq> x3 \<and> ccd x1 x2 x3 \<noteq> x2 \<and> ccd x1 x2 x3 \<noteq> x1)"
            by moura
          then show ?thesis
            using f1 \<open>bound_tx37 = bound_tx37a\<close> \<open>callOrigin4 cca = Some bound_tx37\<close> \<open>calls4 cca = message_content_assign mm content5\<close> \<open>cca \<in> happensBefore4 ccb\<close> f13 f22 f23 f25 f26 f27 by blast
        qed
          
      qed
    qed


next
  case True
  hence [simp]: "i6 = currentInvocation" .
  show ?thesis
    apply (intro allI impI conjI)
    apply (metis True fun_upd_other no_new_transactions_added_to_current no_transaction_in_new_invocation)
    apply (rule_tac x=c0 in exI)
       apply (rule_tac x=c11 in exI)
       apply (rule_tac x=c21 in exI)
    apply (rule_tac x=m2 in exI)
    using c21_freshA a2 apply (auto simp add: callOrigin_def callOrigin_def_2 callOrigin_def_3  calls_def calls_def_2 calls_def_3  
            happensBefore_def_2 happensBefore_def_3 vis_def vis_def_2 transaction_begin_WF_callOrigin_exists tx1_fresh
      split: if_splits)
    using transaction_begin_WF_callOrigin_exists tx1_fresh apply blast
    using before_procedure_invocation_WF_transactionOrigin_exists i_fresh no_new_transactions_added_to_current by blast
qed
qed

end
