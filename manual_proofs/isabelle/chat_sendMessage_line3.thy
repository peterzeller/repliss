
theory chat_sendMessage_line3
  imports Main
begin

      datatype CallId = CallId nat

datatype TxId = TxId nat

datatype ChatId = ChatId nat

datatype MessageId = MessageId nat

datatype String = String nat

datatype UserId = UserId nat

datatype callInfo =
    queryop_chat_exists (key8: "ChatId") (result1: "bool")
  | queryop_message_content_mv_contains (key14: "MessageId") (elem4: "String") (result7: "bool")
  | message_chat_assign (key5: "MessageId") (value2: "ChatId")
  | queryop_chat_messages_contains (key7: "ChatId") (elem2: "MessageId") (result: "bool")
  | chat_messages_add (key: "ChatId") (elem: "MessageId")
  | chat_messages_remove (key1: "ChatId") (elem1: "MessageId")
  | message_content_assign (key4: "MessageId") (value1: "String")
  | message_delete (key6: "MessageId")
  | queryop_message_content_get (key12: "MessageId") (result5: "String")
  | queryop_message_author_getFirst (key10: "MessageId") (result3: "UserId")
  | queryop_message_chat_getFirst (key16: "MessageId") (result9: "ChatId")
  | queryop_message_chat_mv_contains (key17: "MessageId") (elem5: "ChatId") (result10: "bool")
  | queryop_message_chat_get (key15: "MessageId") (result8: "ChatId")
  | no_call 
  | message_author_assign (key3: "MessageId") (qvalue: "UserId")
  | chat_delete (key2: "ChatId")
  | queryop_message_exists (key18: "MessageId") (result11: "bool")
  | queryop_message_content_getFirst (key13: "MessageId") (result6: "String")
  | queryop_message_author_get (key9: "MessageId") (result2: "UserId")
  | queryop_message_author_mv_contains (key11: "MessageId") (elem3: "UserId") (result4: "bool")

datatype InvocationId = InvocationId nat

datatype invocationResult =
    sendMessage_res (sendMessage_res_arg: "MessageId")
  | NoResult 

datatype invocationInfo =
    sendMessage (qfrom: "UserId") (content: "String") (toC: "ChatId")
  | no_invocation 

lemma "sendMessage_line3":
fixes happensBefore3 :: "CallId => CallId set"
         
fixes calls4 :: "CallId => callInfo"
         
fixes knownIds_MessageId1 :: "MessageId set"
         
fixes message_author_mv_contains_res6 :: "MessageId => UserId => bool"
         
fixes transactionOrigin1 :: "TxId => InvocationId option"
         
fixes c11 :: "CallId"
         
fixes invocationRes :: "InvocationId => invocationResult"
         
fixes chat_messages_contains_res3 :: "ChatId => MessageId => bool"
         
fixes message_exists_res4 :: "MessageId => bool"
         
fixes message_author_mv_contains_res :: "MessageId => UserId => bool"
         
fixes message_author_mv_contains_res1 :: "MessageId => UserId => bool"
         
fixes vis1 :: "CallId set"
         
fixes calls1 :: "CallId => callInfo"
         
fixes content_init :: "String"
         
fixes message_author_mv_contains_res5 :: "MessageId => UserId => bool"
         
fixes invocationOp :: "InvocationId => invocationInfo"
         
fixes callOrigin :: "CallId => TxId option"
         
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
         
fixes chat_messages_contains_res2 :: "ChatId => MessageId => bool"
         
fixes c21 :: "CallId"
         
fixes snapshotAddition :: "CallId set"
         
fixes happensBefore1 :: "CallId => CallId set"
         
fixes chat_messages_contains_res5 :: "ChatId => MessageId => bool"
         
fixes generatedIds_MessageId1 :: "MessageId => InvocationId option"
         
fixes generatedIds_MessageId :: "MessageId => InvocationId option"
         
fixes happensBefore5 :: "CallId => CallId set"
         
fixes chat_messages_contains_res :: "ChatId => MessageId => bool"
         
fixes message_exists_res7 :: "MessageId => bool"
         
fixes c31 :: "CallId"
         
fixes message_author_mv_contains_res4 :: "MessageId => UserId => bool"
         
fixes snapshotAddition1 :: "CallId set"
         
fixes currentInvocation :: "InvocationId"
         
fixes message_exists_res2 :: "MessageId => bool"
         
fixes message_exists_res8 :: "MessageId => bool"
         
fixes invocationCalls1 :: "InvocationId => CallId set"
         
fixes c0 :: "CallId"
         
fixes chat_messages_contains_res1 :: "ChatId => MessageId => bool"
         
fixes vis3 :: "CallId set"
         
fixes calls5 :: "CallId => callInfo"
         
fixes knownIds_MessageId :: "MessageId set"
         
fixes message_exists_res :: "MessageId => bool"
         
fixes happensBefore2 :: "CallId => CallId set"
         
fixes message_exists_res1 :: "MessageId => bool"
         
fixes from_init :: "UserId"
         
fixes message_exists_res5 :: "MessageId => bool"
         
fixes message_exists_res9 :: "MessageId => bool"
         
fixes tx :: "TxId"
         
fixes message_author_mv_contains_res7 :: "MessageId => UserId => bool"
         
fixes chat_messages_contains_res4 :: "ChatId => MessageId => bool"
         
fixes toC_init :: "ChatId"
         
fixes message_author_mv_contains_res3 :: "MessageId => UserId => bool"
         
fixes vis4 :: "CallId set"
         
fixes happensBefore :: "CallId => CallId set"
         
fixes newTxns :: "TxId set"
         
assumes before_procedure_invocation_snapshot_addition_transaction_consistent:

        "(\<forall>bound_c116.
         (\<forall>bound_c216.
           (((bound_c116 \<in> snapshotAddition) \<and> (bound_c216 \<in> (happensBefore bound_c116))) \<longrightarrow> (bound_c216 \<in> snapshotAddition))))"
         
assumes before_procedure_invocation_snapshot_addition_transaction_consistent_2:

        "(\<forall>bound_c115.
         (\<forall>bound_c215.
           (((bound_c115 \<in> snapshotAddition) \<and> ((callOrigin bound_c115) = (callOrigin bound_c215)))
             \<longrightarrow> (bound_c215 \<in> snapshotAddition))))"
         
assumes before_procedure_invocation_snapshot_addition_subset_calls:

        "(\<forall>bound_c34. ((bound_c34 \<in> snapshotAddition) \<longrightarrow> ((calls bound_c34) \<noteq> no_call)))"
         
assumes before_procedure_invocation_MessageId_knownIds_are_generated:

        "(\<forall>bound_x3. ((bound_x3 \<in> knownIds_MessageId) \<longrightarrow> \<not>((generatedIds_MessageId bound_x3) = None)))"
         
assumes before_procedure_invocation_message_delete_call_parameter_key_generated:

        "(\<forall>bound_c33.
         (\<forall>bound_key5. (((calls bound_c33) = (message_delete bound_key5)) \<longrightarrow> \<not>((generatedIds_MessageId bound_key5) = None))))"
         
assumes before_procedure_invocation_message_chat_assign_call_parameter_key_generated:

        "(\<forall>bound_c32.
         (\<forall>bound_key4.
           (\<forall>bound_value2.
             (((calls bound_c32) = (message_chat_assign bound_key4 bound_value2))
               \<longrightarrow> \<not>((generatedIds_MessageId bound_key4) = None)))))"
         
assumes before_procedure_invocation_message_content_assign_call_parameter_key_generated:

        "(\<forall>bound_c31.
         (\<forall>bound_key3.
           (\<forall>bound_value1.
             (((calls bound_c31) = (message_content_assign bound_key3 bound_value1))
               \<longrightarrow> \<not>((generatedIds_MessageId bound_key3) = None)))))"
         
assumes before_procedure_invocation_message_author_assign_call_parameter_key_generated:

        "(\<forall>bound_c30.
         (\<forall>bound_key2.
           (\<forall>bound_value.
             (((calls bound_c30) = (message_author_assign bound_key2 bound_value))
               \<longrightarrow> \<not>((generatedIds_MessageId bound_key2) = None)))))"
         
assumes before_procedure_invocation_chat_messages_remove_call_parameter_elem_generated:

        "(\<forall>bound_c20.
         (\<forall>bound_key1.
           (\<forall>bound_elem1.
             (((calls bound_c20) = (chat_messages_remove bound_key1 bound_elem1))
               \<longrightarrow> \<not>((generatedIds_MessageId bound_elem1) = None)))))"
         
assumes before_procedure_invocation_chat_messages_add_call_parameter_elem_generated:

        "(\<forall>bound_c10.
         (\<forall>bound_key.
           (\<forall>bound_elem.
             (((calls bound_c10) = (chat_messages_add bound_key bound_elem)) \<longrightarrow> \<not>((generatedIds_MessageId bound_elem) = None)))))"
         
assumes before_procedure_invocation_sendMessage_result_known:

        "(\<forall>bound_i4.
         (\<forall>bound_result. (((invocationRes bound_i4) = (sendMessage_res bound_result)) \<longrightarrow> (bound_result \<in> knownIds_MessageId))))"
         
assumes before_procedure_invocation_WF_transactionOrigin_exists:

        "(\<forall>bound_tx6.
         (\<forall>bound_i3. (((transactionOrigin bound_tx6) = (Some bound_i3)) \<longrightarrow> ((invocationOp bound_i3) \<noteq> no_invocation))))"
         
assumes before_procedure_invocation_WF_callOrigin_exists:

        "(\<forall>bound_ca1. (\<forall>bound_tx5. (((callOrigin bound_ca1) = (Some bound_tx5)) \<longrightarrow> \<not>((transactionOrigin bound_tx5) = None))))"
         
assumes before_procedure_invocation_WF_no_call_implies_not_in_happensBefore:

        "(\<forall>bound_ca. (\<forall>bound_cb. (((callOrigin bound_ca) = None) \<longrightarrow> \<not>(bound_ca \<in> (happensBefore bound_cb)))))"
         
assumes before_procedure_invocation_WF_no_call_implies_no_happensBefore:

        "(\<forall>bound_c9. (((callOrigin bound_c9) = None) \<longrightarrow> ((happensBefore bound_c9) = {})))"
         
assumes before_procedure_invocation_WF_transactionOrigin_callOrigin:

        "(\<forall>bound_tx4. (((transactionOrigin bound_tx4) = None) \<longrightarrow> (\<forall>bound_c8. ((callOrigin bound_c8) \<noteq> (Some bound_tx4)))))"
         
assumes before_procedure_invocation_WF_callOrigin:

        "(\<forall>bound_c7. (((callOrigin bound_c7) = None) = ((calls bound_c7) = no_call)))"
         
assumes before_procedure_invocation_WF_invocationCalls:

        "(\<forall>bound_i2.
         (\<forall>bound_c6.
           ((bound_c6 \<in> (invocationCalls bound_i2))
             = (\<exists>bound_tx3. (((callOrigin bound_c6) = (Some bound_tx3)) \<and> ((transactionOrigin bound_tx3) = (Some bound_i2)))))))"
         
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

        "(\<forall>bound_i1. (((invocationOp bound_i1) = no_invocation) \<longrightarrow> ((invocationRes bound_i1) = NoReturn)))"
         
assumes before_procedure_invocation_happensBefore_antisym:

        "(\<forall>bound_x1.
         (\<forall>bound_y1. (((bound_x1 \<in> (happensBefore bound_y1)) \<and> (bound_y1 \<in> (happensBefore bound_x1))) \<longrightarrow> (bound_x1 = bound_y1))))"
         
assumes before_procedure_invocation_happensBefore_trans:

        "(\<forall>bound_x.
         (\<forall>bound_y.
           (\<forall>bound_z.
             (((bound_x \<in> (happensBefore bound_y)) \<and> (bound_y \<in> (happensBefore bound_z))) \<longrightarrow> (bound_x \<in> (happensBefore bound_y))))))"
         
assumes before_procedure_invocation_happensBefore_reflex:

        "(\<forall>bound_c5. (((calls bound_c5) \<noteq> no_call) \<longrightarrow> (bound_c5 \<in> (happensBefore bound_c5))))"
         
assumes before_procedure_invocation_visibleCalls_causally_consistent:

        "(\<forall>bound_c114. (\<forall>bound_c214. (((bound_c214 \<in> {}) \<and> (bound_c114 \<in> (happensBefore bound_c214))) \<longrightarrow> (bound_c114 \<in> {}))))"
         
assumes before_procedure_invocation_visibleCalls_transaction_consistent1:

        "(\<forall>bound_c113.
         (\<forall>bound_c213.
           ((((bound_c113 \<in> {}) \<and> ((callOrigin bound_c113) = (callOrigin bound_c213))) \<and> ((calls bound_c213) \<noteq> no_call))
             \<longrightarrow> (bound_c213 \<in> {}))))"
         
assumes before_procedure_invocation_visibleCalls_exist:

        "(\<forall>bound_c4. ((bound_c4 \<in> {}) \<longrightarrow> ((calls bound_c4) \<noteq> no_call)))"
         
assumes before_procedure_invocation_invocation_sequential:

        "(\<forall>bound_c112.
         (\<forall>bound_tx1.
           (\<forall>bound_i.
             (\<forall>bound_c212.
               (\<forall>bound_tx2.
                 ((((((callOrigin bound_c112) = (Some bound_tx1)) \<and> ((transactionOrigin bound_tx1) = (Some bound_i)))
                   \<and> ((callOrigin bound_c212) = (Some bound_tx2)))
                   \<and> ((transactionOrigin bound_tx2) = (Some bound_i)))
                   \<longrightarrow> ((bound_c112 \<in> (happensBefore bound_c212)) \<or> (bound_c212 \<in> (happensBefore bound_c112)))))))))"
         
assumes before_procedure_invocation_happensBefore_exists_r:

        "(\<forall>bound_c111. (\<forall>bound_c211. (((calls bound_c111) = no_call) \<longrightarrow> \<not>(bound_c111 \<in> (happensBefore bound_c211)))))"
         
assumes before_procedure_invocation_happensBefore_exists_l:

        "(\<forall>bound_c110. (\<forall>bound_c210. (((calls bound_c110) = no_call) \<longrightarrow> \<not>(bound_c110 \<in> (happensBefore bound_c210)))))"
         
assumes no_call_in_new_invocation:

        "((invocationCalls currentInvocation) = {})"
         
assumes message_exists_res:

        "(\<forall>bound_m2.
         ((message_exists_res bound_m2)
           = (\<exists>bound_c14.
           (((((bound_c14 \<in> snapshotAddition)
             \<and> (\<exists>bound_args6. ((calls bound_c14) = (message_author_assign bound_m2 bound_args6))))
             \<or> ((bound_c14 \<in> snapshotAddition)
               \<and> (\<exists>bound_args7. ((calls bound_c14) = (message_content_assign bound_m2 bound_args7)))))
             \<or> ((bound_c14 \<in> snapshotAddition)
               \<and> (\<exists>bound_args8. ((calls bound_c14) = (message_chat_assign bound_m2 bound_args8)))))
             \<and> (\<forall>bound_c24.
               (((bound_c24 \<in> snapshotAddition) \<and> ((calls bound_c24) = (message_delete bound_m2)))
                 \<longrightarrow> (bound_c24 \<in> (happensBefore bound_c14))))))))"
         
assumes message_author_mv_contains_res:

        "(\<forall>bound_m2.
         (\<forall>bound_a1.
           (((message_author_mv_contains_res bound_m2) bound_a1)
             = (\<exists>bound_c15.
             (((bound_c15 \<in> snapshotAddition)
               \<and> (((calls bound_c15) = (message_author_assign bound_m2 bound_a1))
                 \<and> (\<forall>bound_d4.
                   (((bound_d4 \<in> snapshotAddition) \<and> ((calls bound_d4) = (message_delete bound_m2)))
                     \<longrightarrow> (bound_d4 \<in> (happensBefore bound_c15))))))
               \<and> \<not>(\<exists>bound_c25.
                 (\<exists>bound_anyArgs.
                   ((((bound_c25 \<in> snapshotAddition) \<and> (bound_c15 \<noteq> bound_c25))
                     \<and> (((calls bound_c25) = (message_author_assign bound_m2 bound_anyArgs))
                       \<and> (\<forall>bound_d5.
                         (((bound_d5 \<in> snapshotAddition) \<and> ((calls bound_d5) = (message_delete bound_m2)))
                           \<longrightarrow> (bound_d5 \<in> (happensBefore bound_c25))))))
                     \<and> (bound_c15 \<in> (happensBefore bound_c25))))))))))"
         
assumes message_author_mv_contains_res_2:

        "(\<forall>bound_m2.
         (\<forall>bound_a2.
           (((message_author_mv_contains_res1 bound_m2) bound_a2)
             = (\<exists>bound_c16.
             (((bound_c16 \<in> snapshotAddition)
               \<and> (((calls bound_c16) = (message_author_assign bound_m2 bound_a2))
                 \<and> (\<forall>bound_d6.
                   (((bound_d6 \<in> snapshotAddition) \<and> ((calls bound_d6) = (message_delete bound_m2)))
                     \<longrightarrow> (bound_d6 \<in> (happensBefore bound_c16))))))
               \<and> \<not>(\<exists>bound_c26.
                 (\<exists>bound_anyArgs1.
                   ((((bound_c26 \<in> snapshotAddition) \<and> (bound_c16 \<noteq> bound_c26))
                     \<and> (((calls bound_c26) = (message_author_assign bound_m2 bound_anyArgs1))
                       \<and> (\<forall>bound_d7.
                         (((bound_d7 \<in> snapshotAddition) \<and> ((calls bound_d7) = (message_delete bound_m2)))
                           \<longrightarrow> (bound_d7 \<in> (happensBefore bound_c26))))))
                     \<and> (bound_c16 \<in> (happensBefore bound_c26))))))))))"
         
assumes message_exists_res_2:

        "(\<forall>bound_m3.
         ((message_exists_res1 bound_m3)
           = (\<exists>bound_c17.
           (((((bound_c17 \<in> ({} \<union> snapshotAddition))
             \<and> (\<exists>bound_args9. ((calls bound_c17) = (message_author_assign bound_m3 bound_args9))))
             \<or> ((bound_c17 \<in> ({} \<union> snapshotAddition))
               \<and> (\<exists>bound_args10. ((calls bound_c17) = (message_content_assign bound_m3 bound_args10)))))
             \<or> ((bound_c17 \<in> ({} \<union> snapshotAddition))
               \<and> (\<exists>bound_args11. ((calls bound_c17) = (message_chat_assign bound_m3 bound_args11)))))
             \<and> (\<forall>bound_c27.
               (((bound_c27 \<in> ({} \<union> snapshotAddition)) \<and> ((calls bound_c27) = (message_delete bound_m3)))
                 \<longrightarrow> (bound_c27 \<in> (happensBefore bound_c17))))))))"
         
assumes message_author_mv_contains_res_3:

        "(\<forall>bound_m3.
         (\<forall>bound_a11.
           (((message_author_mv_contains_res2 bound_m3) bound_a11)
             = (\<exists>bound_c18.
             (((bound_c18 \<in> ({} \<union> snapshotAddition))
               \<and> (((calls bound_c18) = (message_author_assign bound_m3 bound_a11))
                 \<and> (\<forall>bound_d8.
                   (((bound_d8 \<in> ({} \<union> snapshotAddition)) \<and> ((calls bound_d8) = (message_delete bound_m3)))
                     \<longrightarrow> (bound_d8 \<in> (happensBefore bound_c18))))))
               \<and> \<not>(\<exists>bound_c28.
                 (\<exists>bound_anyArgs2.
                   ((((bound_c28 \<in> ({} \<union> snapshotAddition)) \<and> (bound_c18 \<noteq> bound_c28))
                     \<and> (((calls bound_c28) = (message_author_assign bound_m3 bound_anyArgs2))
                       \<and> (\<forall>bound_d9.
                         (((bound_d9 \<in> ({} \<union> snapshotAddition)) \<and> ((calls bound_d9) = (message_delete bound_m3)))
                           \<longrightarrow> (bound_d9 \<in> (happensBefore bound_c28))))))
                     \<and> (bound_c18 \<in> (happensBefore bound_c28))))))))))"
         
assumes message_author_mv_contains_res_4:

        "(\<forall>bound_m3.
         (\<forall>bound_a21.
           (((message_author_mv_contains_res3 bound_m3) bound_a21)
             = (\<exists>bound_c19.
             (((bound_c19 \<in> ({} \<union> snapshotAddition))
               \<and> (((calls bound_c19) = (message_author_assign bound_m3 bound_a21))
                 \<and> (\<forall>bound_d10.
                   (((bound_d10 \<in> ({} \<union> snapshotAddition)) \<and> ((calls bound_d10) = (message_delete bound_m3)))
                     \<longrightarrow> (bound_d10 \<in> (happensBefore bound_c19))))))
               \<and> \<not>(\<exists>bound_c29.
                 (\<exists>bound_anyArgs3.
                   ((((bound_c29 \<in> ({} \<union> snapshotAddition)) \<and> (bound_c19 \<noteq> bound_c29))
                     \<and> (((calls bound_c29) = (message_author_assign bound_m3 bound_anyArgs3))
                       \<and> (\<forall>bound_d11.
                         (((bound_d11 \<in> ({} \<union> snapshotAddition)) \<and> ((calls bound_d11) = (message_delete bound_m3)))
                           \<longrightarrow> (bound_d11 \<in> (happensBefore bound_c29))))))
                     \<and> (bound_c19 \<in> (happensBefore bound_c29))))))))))"
         
assumes before_procedure_invocation_invariant_1:

        "((\<forall>bound_m2.
         (\<forall>bound_a1.
           (\<forall>bound_a2.
             ((((message_exists_res bound_m2) \<and> ((message_author_mv_contains_res bound_m2) bound_a1))
               \<and> ((message_author_mv_contains_res1 bound_m2) bound_a2))
               \<longrightarrow> (bound_a1 = bound_a2)))))
         \<and> (\<forall>bound_m3.
           (\<forall>bound_a11.
             (\<forall>bound_a21.
               ((((message_exists_res1 bound_m3) \<and> ((message_author_mv_contains_res2 bound_m3) bound_a11))
                 \<and> ((message_author_mv_contains_res3 bound_m3) bound_a21))
                 \<longrightarrow> (bound_a11 = bound_a21))))))"
         
assumes chat_messages_contains_res:

        "(\<forall>bound_c.
         (\<forall>bound_m.
           (((chat_messages_contains_res bound_c) bound_m)
             = (\<exists>bound_c1.
             (((bound_c1 \<in> snapshotAddition)
               \<and> (((calls bound_c1) = (chat_messages_add bound_c bound_m))
                 \<and> (\<forall>bound_d.
                   (((bound_d \<in> snapshotAddition) \<and> ((calls bound_d) = (chat_delete bound_c)))
                     \<longrightarrow> (bound_d \<in> (happensBefore bound_c1))))))
               \<and> (\<forall>bound_c2.
                 (((bound_c2 \<in> snapshotAddition)
                   \<and> (((calls bound_c2) = (chat_messages_remove bound_c bound_m))
                     \<and> (\<forall>bound_d1.
                       (((bound_d1 \<in> snapshotAddition) \<and> ((calls bound_d1) = (chat_delete bound_c)))
                         \<longrightarrow> (bound_d1 \<in> (happensBefore bound_c2))))))
                   \<longrightarrow> (bound_c2 \<in> (happensBefore bound_c1)))))))))"
         
assumes message_exists_res_3:

        "(\<forall>bound_m.
         ((message_exists_res2 bound_m)
           = (\<exists>bound_c11.
           (((((bound_c11 \<in> snapshotAddition) \<and> (\<exists>bound_args. ((calls bound_c11) = (message_author_assign bound_m bound_args))))
             \<or> ((bound_c11 \<in> snapshotAddition)
               \<and> (\<exists>bound_args1. ((calls bound_c11) = (message_content_assign bound_m bound_args1)))))
             \<or> ((bound_c11 \<in> snapshotAddition)
               \<and> (\<exists>bound_args2. ((calls bound_c11) = (message_chat_assign bound_m bound_args2)))))
             \<and> (\<forall>bound_c21.
               (((bound_c21 \<in> snapshotAddition) \<and> ((calls bound_c21) = (message_delete bound_m)))
                 \<longrightarrow> (bound_c21 \<in> (happensBefore bound_c11))))))))"
         
assumes chat_messages_contains_res_2:

        "(\<forall>bound_c3.
         (\<forall>bound_m1.
           (((chat_messages_contains_res1 bound_c3) bound_m1)
             = (\<exists>bound_c12.
             (((bound_c12 \<in> ({} \<union> snapshotAddition))
               \<and> (((calls bound_c12) = (chat_messages_add bound_c3 bound_m1))
                 \<and> (\<forall>bound_d2.
                   (((bound_d2 \<in> ({} \<union> snapshotAddition)) \<and> ((calls bound_d2) = (chat_delete bound_c3)))
                     \<longrightarrow> (bound_d2 \<in> (happensBefore bound_c12))))))
               \<and> (\<forall>bound_c22.
                 (((bound_c22 \<in> ({} \<union> snapshotAddition))
                   \<and> (((calls bound_c22) = (chat_messages_remove bound_c3 bound_m1))
                     \<and> (\<forall>bound_d3.
                       (((bound_d3 \<in> ({} \<union> snapshotAddition)) \<and> ((calls bound_d3) = (chat_delete bound_c3)))
                         \<longrightarrow> (bound_d3 \<in> (happensBefore bound_c22))))))
                   \<longrightarrow> (bound_c22 \<in> (happensBefore bound_c12)))))))))"
         
assumes message_exists_res_4:

        "(\<forall>bound_m1.
         ((message_exists_res3 bound_m1)
           = (\<exists>bound_c13.
           (((((bound_c13 \<in> ({} \<union> snapshotAddition))
             \<and> (\<exists>bound_args3. ((calls bound_c13) = (message_author_assign bound_m1 bound_args3))))
             \<or> ((bound_c13 \<in> ({} \<union> snapshotAddition))
               \<and> (\<exists>bound_args4. ((calls bound_c13) = (message_content_assign bound_m1 bound_args4)))))
             \<or> ((bound_c13 \<in> ({} \<union> snapshotAddition))
               \<and> (\<exists>bound_args5. ((calls bound_c13) = (message_chat_assign bound_m1 bound_args5)))))
             \<and> (\<forall>bound_c23.
               (((bound_c23 \<in> ({} \<union> snapshotAddition)) \<and> ((calls bound_c23) = (message_delete bound_m1)))
                 \<longrightarrow> (bound_c23 \<in> (happensBefore bound_c13))))))))"
         
assumes before_procedure_invocation_invariant_0:

        "((\<forall>bound_c. (\<forall>bound_m. (((chat_messages_contains_res bound_c) bound_m) \<longrightarrow> (message_exists_res2 bound_m))))
         \<and> (\<forall>bound_c3. (\<forall>bound_m1. (((chat_messages_contains_res1 bound_c3) bound_m1) \<longrightarrow> (message_exists_res3 bound_m1)))))"
         
assumes i_fresh:

        "((invocationOp currentInvocation) = no_invocation)"
         
assumes old_transactions_unchanged:

        "(\<forall>c5.
         (\<forall>tx3.
           (((((calls c5) = no_call) \<and> ((calls1 c5) \<noteq> no_call)) \<and> ((callOrigin1 c5) = (Some tx3)))
             \<longrightarrow> ((transactionOrigin tx3) = None))))"
         
assumes growth_invocation_res:

        "(\<forall>i1. (((invocationRes i1) \<noteq> NoReturn) \<longrightarrow> ((invocationRes i1) = (invocationRes i1))))"
         
assumes growth_invocation_op:

        "(\<forall>i.
         ((((invocationOp(currentInvocation := (sendMessage from_init content_init toC_init))) i) \<noteq> no_invocation)
           \<longrightarrow> (((invocationOp(currentInvocation := (sendMessage from_init content_init toC_init))) i)
             = ((invocationOp(currentInvocation := (sendMessage from_init content_init toC_init))) i))))"
         
assumes growth_tx_origin:

        "(\<forall>tx2. (\<not>((transactionOrigin tx2) = None) \<longrightarrow> ((transactionOrigin1 tx2) = (transactionOrigin tx2))))"
         
assumes growth_call_tx:

        "(\<forall>c4. (((calls c4) \<noteq> no_call) \<longrightarrow> ((callOrigin1 c4) = (callOrigin c4))))"
         
assumes growth_happensbefore:

        "(\<forall>c3. (((calls c3) \<noteq> no_call) \<longrightarrow> ((happensBefore1 c3) = (happensBefore c3))))"
         
assumes growth_calls:

        "(\<forall>c2. (((calls c2) \<noteq> no_call) \<longrightarrow> ((calls1 c2) = (calls c2))))"
         
assumes growth_visible_calls:

        "(\<forall>c1. ((c1 \<in> {}) \<longrightarrow> (c1 \<in> {})))"
         
assumes growth_callOrigin:

        "(\<forall>c. (\<forall>tx1. (((callOrigin c) = (Some tx1)) \<longrightarrow> ((callOrigin1 c) = (Some tx1)))))"
         
assumes transaction_begin_snapshot_addition_transaction_consistent:

        "(\<forall>bound_c143.
         (\<forall>bound_c243.
           (((bound_c143 \<in> snapshotAddition1) \<and> (bound_c243 \<in> (happensBefore1 bound_c143))) \<longrightarrow> (bound_c243 \<in> snapshotAddition1))))"
         
assumes transaction_begin_snapshot_addition_transaction_consistent_2:

        "(\<forall>bound_c142.
         (\<forall>bound_c242.
           (((bound_c142 \<in> snapshotAddition1) \<and> ((callOrigin1 bound_c142) = (callOrigin1 bound_c242)))
             \<longrightarrow> (bound_c242 \<in> snapshotAddition1))))"
         
assumes transaction_begin_snapshot_addition_subset_calls:

        "(\<forall>bound_c51. ((bound_c51 \<in> snapshotAddition1) \<longrightarrow> ((calls1 bound_c51) \<noteq> no_call)))"
         
assumes transaction_begin_MessageId_knownIds_are_generated:

        "(\<forall>bound_x6. ((bound_x6 \<in> knownIds_MessageId1) \<longrightarrow> \<not>((generatedIds_MessageId1 bound_x6) = None)))"
         
assumes transaction_begin_message_delete_call_parameter_key_generated:

        "(\<forall>bound_c50.
         (\<forall>bound_key11. (((calls1 bound_c50) = (message_delete bound_key11)) \<longrightarrow> \<not>((generatedIds_MessageId1 bound_key11) = None))))"
         
assumes transaction_begin_message_chat_assign_call_parameter_key_generated:

        "(\<forall>bound_c49.
         (\<forall>bound_key10.
           (\<forall>bound_value5.
             (((calls1 bound_c49) = (message_chat_assign bound_key10 bound_value5))
               \<longrightarrow> \<not>((generatedIds_MessageId1 bound_key10) = None)))))"
         
assumes transaction_begin_message_content_assign_call_parameter_key_generated:

        "(\<forall>bound_c48.
         (\<forall>bound_key9.
           (\<forall>bound_value4.
             (((calls1 bound_c48) = (message_content_assign bound_key9 bound_value4))
               \<longrightarrow> \<not>((generatedIds_MessageId1 bound_key9) = None)))))"
         
assumes transaction_begin_message_author_assign_call_parameter_key_generated:

        "(\<forall>bound_c47.
         (\<forall>bound_key8.
           (\<forall>bound_value3.
             (((calls1 bound_c47) = (message_author_assign bound_key8 bound_value3))
               \<longrightarrow> \<not>((generatedIds_MessageId1 bound_key8) = None)))))"
         
assumes transaction_begin_chat_messages_remove_call_parameter_elem_generated:

        "(\<forall>bound_c46.
         (\<forall>bound_key7.
           (\<forall>bound_elem3.
             (((calls1 bound_c46) = (chat_messages_remove bound_key7 bound_elem3))
               \<longrightarrow> \<not>((generatedIds_MessageId1 bound_elem3) = None)))))"
         
assumes transaction_begin_chat_messages_add_call_parameter_elem_generated:

        "(\<forall>bound_c45.
         (\<forall>bound_key6.
           (\<forall>bound_elem2.
             (((calls1 bound_c45) = (chat_messages_add bound_key6 bound_elem2))
               \<longrightarrow> \<not>((generatedIds_MessageId1 bound_elem2) = None)))))"
         
assumes transaction_begin_sendMessage_result_known:

        "(\<forall>bound_i9.
         (\<forall>bound_result1.
           (((invocationRes bound_i9) = (sendMessage_res bound_result1)) \<longrightarrow> (bound_result1 \<in> knownIds_MessageId1))))"
         
assumes transaction_begin_WF_transactionOrigin_exists:

        "(\<forall>bound_tx10.
         (\<forall>bound_i8.
           (((transactionOrigin1 bound_tx10) = (Some bound_i8))
             \<longrightarrow> (((invocationOp(currentInvocation := (sendMessage from_init content_init toC_init))) bound_i8) \<noteq> no_invocation))))"
         
assumes transaction_begin_WF_callOrigin_exists:

        "(\<forall>bound_ca3. (\<forall>bound_tx9. (((callOrigin1 bound_ca3) = (Some bound_tx9)) \<longrightarrow> \<not>((transactionOrigin1 bound_tx9) = None))))"
         
assumes transaction_begin_WF_no_call_implies_not_in_happensBefore:

        "(\<forall>bound_ca2. (\<forall>bound_cb1. (((callOrigin1 bound_ca2) = None) \<longrightarrow> \<not>(bound_ca2 \<in> (happensBefore1 bound_cb1)))))"
         
assumes transaction_begin_WF_no_call_implies_no_happensBefore:

        "(\<forall>bound_c44. (((callOrigin1 bound_c44) = None) \<longrightarrow> ((happensBefore1 bound_c44) = {})))"
         
assumes transaction_begin_WF_transactionOrigin_callOrigin:

        "(\<forall>bound_tx8. (((transactionOrigin1 bound_tx8) = None) \<longrightarrow> (\<forall>bound_c43. ((callOrigin1 bound_c43) \<noteq> (Some bound_tx8)))))"
         
assumes transaction_begin_WF_callOrigin:

        "(\<forall>bound_c42. (((callOrigin1 bound_c42) = None) = ((calls1 bound_c42) = no_call)))"
         
assumes transaction_begin_WF_invocationCalls:

        "(\<forall>bound_i7.
         (\<forall>bound_c41.
           ((bound_c41 \<in> (invocationCalls1 bound_i7))
             = (\<exists>bound_tx7. (((callOrigin1 bound_c41) = (Some bound_tx7)) \<and> ((transactionOrigin1 bound_tx7) = (Some bound_i7)))))))"
         
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

        "(\<forall>bound_i6.
         ((((invocationOp(currentInvocation := (sendMessage from_init content_init toC_init))) bound_i6) = no_invocation)
           \<longrightarrow> ((invocationRes bound_i6) = NoReturn)))"
         
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

        "(\<forall>bound_c40. (((calls1 bound_c40) \<noteq> no_call) \<longrightarrow> (bound_c40 \<in> (happensBefore1 bound_c40))))"
         
assumes transaction_begin_visibleCalls_causally_consistent:

        "(\<forall>bound_c141. (\<forall>bound_c241. (((bound_c241 \<in> vis) \<and> (bound_c141 \<in> (happensBefore1 bound_c241))) \<longrightarrow> (bound_c141 \<in> vis))))"
         
assumes transaction_begin_visibleCalls_transaction_consistent1:

        "(\<forall>bound_c140.
         (\<forall>bound_c240.
           ((((bound_c140 \<in> vis) \<and> ((callOrigin1 bound_c140) = (callOrigin1 bound_c240))) \<and> ((calls1 bound_c240) \<noteq> no_call))
             \<longrightarrow> (bound_c240 \<in> vis))))"
         
assumes transaction_begin_visibleCalls_exist:

        "(\<forall>bound_c39. ((bound_c39 \<in> vis) \<longrightarrow> ((calls1 bound_c39) \<noteq> no_call)))"
         
assumes transaction_begin_invocation_sequential:

        "(\<forall>bound_c139.
         (\<forall>bound_tx11.
           (\<forall>bound_i5.
             (\<forall>bound_c239.
               (\<forall>bound_tx21.
                 ((((((callOrigin1 bound_c139) = (Some bound_tx11)) \<and> ((transactionOrigin1 bound_tx11) = (Some bound_i5)))
                   \<and> ((callOrigin1 bound_c239) = (Some bound_tx21)))
                   \<and> ((transactionOrigin1 bound_tx21) = (Some bound_i5)))
                   \<longrightarrow> ((bound_c139 \<in> (happensBefore1 bound_c239)) \<or> (bound_c239 \<in> (happensBefore1 bound_c139)))))))))"
         
assumes transaction_begin_happensBefore_exists_r:

        "(\<forall>bound_c138. (\<forall>bound_c238. (((calls1 bound_c138) = no_call) \<longrightarrow> \<not>(bound_c138 \<in> (happensBefore1 bound_c238)))))"
         
assumes transaction_begin_happensBefore_exists_l:

        "(\<forall>bound_c137. (\<forall>bound_c237. (((calls1 bound_c137) = no_call) \<longrightarrow> \<not>(bound_c137 \<in> (happensBefore1 bound_c237)))))"
         
assumes message_exists_res_5:

        "(\<forall>bound_m10.
         ((message_exists_res4 bound_m10)
           = (\<exists>bound_c131.
           (((((bound_c131 \<in> snapshotAddition1)
             \<and> (\<exists>bound_args30. ((calls1 bound_c131) = (message_author_assign bound_m10 bound_args30))))
             \<or> ((bound_c131 \<in> snapshotAddition1)
               \<and> (\<exists>bound_args31. ((calls1 bound_c131) = (message_content_assign bound_m10 bound_args31)))))
             \<or> ((bound_c131 \<in> snapshotAddition1)
               \<and> (\<exists>bound_args32. ((calls1 bound_c131) = (message_chat_assign bound_m10 bound_args32)))))
             \<and> (\<forall>bound_c231.
               (((bound_c231 \<in> snapshotAddition1) \<and> ((calls1 bound_c231) = (message_delete bound_m10)))
                 \<longrightarrow> (bound_c231 \<in> (happensBefore1 bound_c131))))))))"
         
assumes message_author_mv_contains_res_5:

        "(\<forall>bound_m10.
         (\<forall>bound_a14.
           (((message_author_mv_contains_res4 bound_m10) bound_a14)
             = (\<exists>bound_c132.
             (((bound_c132 \<in> snapshotAddition1)
               \<and> (((calls1 bound_c132) = (message_author_assign bound_m10 bound_a14))
                 \<and> (\<forall>bound_d28.
                   (((bound_d28 \<in> snapshotAddition1) \<and> ((calls1 bound_d28) = (message_delete bound_m10)))
                     \<longrightarrow> (bound_d28 \<in> (happensBefore1 bound_c132))))))
               \<and> \<not>(\<exists>bound_c232.
                 (\<exists>bound_anyArgs8.
                   ((((bound_c232 \<in> snapshotAddition1) \<and> (bound_c132 \<noteq> bound_c232))
                     \<and> (((calls1 bound_c232) = (message_author_assign bound_m10 bound_anyArgs8))
                       \<and> (\<forall>bound_d29.
                         (((bound_d29 \<in> snapshotAddition1) \<and> ((calls1 bound_d29) = (message_delete bound_m10)))
                           \<longrightarrow> (bound_d29 \<in> (happensBefore1 bound_c232))))))
                     \<and> (bound_c132 \<in> (happensBefore1 bound_c232))))))))))"
         
assumes message_author_mv_contains_res_6:

        "(\<forall>bound_m10.
         (\<forall>bound_a24.
           (((message_author_mv_contains_res5 bound_m10) bound_a24)
             = (\<exists>bound_c133.
             (((bound_c133 \<in> snapshotAddition1)
               \<and> (((calls1 bound_c133) = (message_author_assign bound_m10 bound_a24))
                 \<and> (\<forall>bound_d30.
                   (((bound_d30 \<in> snapshotAddition1) \<and> ((calls1 bound_d30) = (message_delete bound_m10)))
                     \<longrightarrow> (bound_d30 \<in> (happensBefore1 bound_c133))))))
               \<and> \<not>(\<exists>bound_c233.
                 (\<exists>bound_anyArgs9.
                   ((((bound_c233 \<in> snapshotAddition1) \<and> (bound_c133 \<noteq> bound_c233))
                     \<and> (((calls1 bound_c233) = (message_author_assign bound_m10 bound_anyArgs9))
                       \<and> (\<forall>bound_d31.
                         (((bound_d31 \<in> snapshotAddition1) \<and> ((calls1 bound_d31) = (message_delete bound_m10)))
                           \<longrightarrow> (bound_d31 \<in> (happensBefore1 bound_c233))))))
                     \<and> (bound_c133 \<in> (happensBefore1 bound_c233))))))))))"
         
assumes message_exists_res_6:

        "(\<forall>bound_m11.
         ((message_exists_res5 bound_m11)
           = (\<exists>bound_c134.
           (((((bound_c134 \<in> (vis \<union> snapshotAddition1))
             \<and> (\<exists>bound_args33. ((calls1 bound_c134) = (message_author_assign bound_m11 bound_args33))))
             \<or> ((bound_c134 \<in> (vis \<union> snapshotAddition1))
               \<and> (\<exists>bound_args34. ((calls1 bound_c134) = (message_content_assign bound_m11 bound_args34)))))
             \<or> ((bound_c134 \<in> (vis \<union> snapshotAddition1))
               \<and> (\<exists>bound_args35. ((calls1 bound_c134) = (message_chat_assign bound_m11 bound_args35)))))
             \<and> (\<forall>bound_c234.
               (((bound_c234 \<in> (vis \<union> snapshotAddition1)) \<and> ((calls1 bound_c234) = (message_delete bound_m11)))
                 \<longrightarrow> (bound_c234 \<in> (happensBefore1 bound_c134))))))))"
         
assumes message_author_mv_contains_res_7:

        "(\<forall>bound_m11.
         (\<forall>bound_a15.
           (((message_author_mv_contains_res6 bound_m11) bound_a15)
             = (\<exists>bound_c135.
             (((bound_c135 \<in> (vis \<union> snapshotAddition1))
               \<and> (((calls1 bound_c135) = (message_author_assign bound_m11 bound_a15))
                 \<and> (\<forall>bound_d32.
                   (((bound_d32 \<in> (vis \<union> snapshotAddition1)) \<and> ((calls1 bound_d32) = (message_delete bound_m11)))
                     \<longrightarrow> (bound_d32 \<in> (happensBefore1 bound_c135))))))
               \<and> \<not>(\<exists>bound_c235.
                 (\<exists>bound_anyArgs10.
                   ((((bound_c235 \<in> (vis \<union> snapshotAddition1)) \<and> (bound_c135 \<noteq> bound_c235))
                     \<and> (((calls1 bound_c235) = (message_author_assign bound_m11 bound_anyArgs10))
                       \<and> (\<forall>bound_d33.
                         (((bound_d33 \<in> (vis \<union> snapshotAddition1)) \<and> ((calls1 bound_d33) = (message_delete bound_m11)))
                           \<longrightarrow> (bound_d33 \<in> (happensBefore1 bound_c235))))))
                     \<and> (bound_c135 \<in> (happensBefore1 bound_c235))))))))))"
         
assumes message_author_mv_contains_res_8:

        "(\<forall>bound_m11.
         (\<forall>bound_a25.
           (((message_author_mv_contains_res7 bound_m11) bound_a25)
             = (\<exists>bound_c136.
             (((bound_c136 \<in> (vis \<union> snapshotAddition1))
               \<and> (((calls1 bound_c136) = (message_author_assign bound_m11 bound_a25))
                 \<and> (\<forall>bound_d34.
                   (((bound_d34 \<in> (vis \<union> snapshotAddition1)) \<and> ((calls1 bound_d34) = (message_delete bound_m11)))
                     \<longrightarrow> (bound_d34 \<in> (happensBefore1 bound_c136))))))
               \<and> \<not>(\<exists>bound_c236.
                 (\<exists>bound_anyArgs11.
                   ((((bound_c236 \<in> (vis \<union> snapshotAddition1)) \<and> (bound_c136 \<noteq> bound_c236))
                     \<and> (((calls1 bound_c236) = (message_author_assign bound_m11 bound_anyArgs11))
                       \<and> (\<forall>bound_d35.
                         (((bound_d35 \<in> (vis \<union> snapshotAddition1)) \<and> ((calls1 bound_d35) = (message_delete bound_m11)))
                           \<longrightarrow> (bound_d35 \<in> (happensBefore1 bound_c236))))))
                     \<and> (bound_c136 \<in> (happensBefore1 bound_c236))))))))))"
         
assumes at_transaction_begin_invariant_1:

        "((\<forall>bound_m10.
         (\<forall>bound_a14.
           (\<forall>bound_a24.
             ((((message_exists_res4 bound_m10) \<and> ((message_author_mv_contains_res4 bound_m10) bound_a14))
               \<and> ((message_author_mv_contains_res5 bound_m10) bound_a24))
               \<longrightarrow> (bound_a14 = bound_a24)))))
         \<and> (\<forall>bound_m11.
           (\<forall>bound_a15.
             (\<forall>bound_a25.
               ((((message_exists_res5 bound_m11) \<and> ((message_author_mv_contains_res6 bound_m11) bound_a15))
                 \<and> ((message_author_mv_contains_res7 bound_m11) bound_a25))
                 \<longrightarrow> (bound_a15 = bound_a25))))))"
         
assumes chat_messages_contains_res_3:

        "(\<forall>bound_c37.
         (\<forall>bound_m8.
           (((chat_messages_contains_res2 bound_c37) bound_m8)
             = (\<exists>bound_c127.
             (((bound_c127 \<in> snapshotAddition1)
               \<and> (((calls1 bound_c127) = (chat_messages_add bound_c37 bound_m8))
                 \<and> (\<forall>bound_d24.
                   (((bound_d24 \<in> snapshotAddition1) \<and> ((calls1 bound_d24) = (chat_delete bound_c37)))
                     \<longrightarrow> (bound_d24 \<in> (happensBefore1 bound_c127))))))
               \<and> (\<forall>bound_c227.
                 (((bound_c227 \<in> snapshotAddition1)
                   \<and> (((calls1 bound_c227) = (chat_messages_remove bound_c37 bound_m8))
                     \<and> (\<forall>bound_d25.
                       (((bound_d25 \<in> snapshotAddition1) \<and> ((calls1 bound_d25) = (chat_delete bound_c37)))
                         \<longrightarrow> (bound_d25 \<in> (happensBefore1 bound_c227))))))
                   \<longrightarrow> (bound_c227 \<in> (happensBefore1 bound_c127)))))))))"
         
assumes message_exists_res_7:

        "(\<forall>bound_m8.
         ((message_exists_res6 bound_m8)
           = (\<exists>bound_c128.
           (((((bound_c128 \<in> snapshotAddition1)
             \<and> (\<exists>bound_args24. ((calls1 bound_c128) = (message_author_assign bound_m8 bound_args24))))
             \<or> ((bound_c128 \<in> snapshotAddition1)
               \<and> (\<exists>bound_args25. ((calls1 bound_c128) = (message_content_assign bound_m8 bound_args25)))))
             \<or> ((bound_c128 \<in> snapshotAddition1)
               \<and> (\<exists>bound_args26. ((calls1 bound_c128) = (message_chat_assign bound_m8 bound_args26)))))
             \<and> (\<forall>bound_c228.
               (((bound_c228 \<in> snapshotAddition1) \<and> ((calls1 bound_c228) = (message_delete bound_m8)))
                 \<longrightarrow> (bound_c228 \<in> (happensBefore1 bound_c128))))))))"
         
assumes chat_messages_contains_res_4:

        "(\<forall>bound_c38.
         (\<forall>bound_m9.
           (((chat_messages_contains_res3 bound_c38) bound_m9)
             = (\<exists>bound_c129.
             (((bound_c129 \<in> (vis \<union> snapshotAddition1))
               \<and> (((calls1 bound_c129) = (chat_messages_add bound_c38 bound_m9))
                 \<and> (\<forall>bound_d26.
                   (((bound_d26 \<in> (vis \<union> snapshotAddition1)) \<and> ((calls1 bound_d26) = (chat_delete bound_c38)))
                     \<longrightarrow> (bound_d26 \<in> (happensBefore1 bound_c129))))))
               \<and> (\<forall>bound_c229.
                 (((bound_c229 \<in> (vis \<union> snapshotAddition1))
                   \<and> (((calls1 bound_c229) = (chat_messages_remove bound_c38 bound_m9))
                     \<and> (\<forall>bound_d27.
                       (((bound_d27 \<in> (vis \<union> snapshotAddition1)) \<and> ((calls1 bound_d27) = (chat_delete bound_c38)))
                         \<longrightarrow> (bound_d27 \<in> (happensBefore1 bound_c229))))))
                   \<longrightarrow> (bound_c229 \<in> (happensBefore1 bound_c129)))))))))"
         
assumes message_exists_res_8:

        "(\<forall>bound_m9.
         ((message_exists_res7 bound_m9)
           = (\<exists>bound_c130.
           (((((bound_c130 \<in> (vis \<union> snapshotAddition1))
             \<and> (\<exists>bound_args27. ((calls1 bound_c130) = (message_author_assign bound_m9 bound_args27))))
             \<or> ((bound_c130 \<in> (vis \<union> snapshotAddition1))
               \<and> (\<exists>bound_args28. ((calls1 bound_c130) = (message_content_assign bound_m9 bound_args28)))))
             \<or> ((bound_c130 \<in> (vis \<union> snapshotAddition1))
               \<and> (\<exists>bound_args29. ((calls1 bound_c130) = (message_chat_assign bound_m9 bound_args29)))))
             \<and> (\<forall>bound_c230.
               (((bound_c230 \<in> (vis \<union> snapshotAddition1)) \<and> ((calls1 bound_c230) = (message_delete bound_m9)))
                 \<longrightarrow> (bound_c230 \<in> (happensBefore1 bound_c130))))))))"
         
assumes at_transaction_begin_invariant_0:

        "((\<forall>bound_c37. (\<forall>bound_m8. (((chat_messages_contains_res2 bound_c37) bound_m8) \<longrightarrow> (message_exists_res6 bound_m8))))
         \<and> (\<forall>bound_c38. (\<forall>bound_m9. (((chat_messages_contains_res3 bound_c38) bound_m9) \<longrightarrow> (message_exists_res7 bound_m9)))))"
         
assumes no_new_calls_addded_to_current:

        "((invocationCalls currentInvocation) = (invocationCalls1 currentInvocation))"
         
assumes no_new_transactions_added_to_current:

        "(\<forall>bound_t.
         (((transactionOrigin bound_t) = (Some currentInvocation)) = ((transactionOrigin1 bound_t) = (Some currentInvocation))))"
         
assumes tx_fresh:

        "((transactionOrigin1 tx) = None)"
         
assumes vis_update:

        "(vis = ({} \<union> newCalls))"
         
assumes new_transactions_exist:

        "(newTxns \<subseteq> (dom transactionOrigin1))"
         
assumes m_new_id_fresh:

        "((generatedIds_MessageId1 m) = None)"
         
assumes c0_freshB:

        "((calls1 c0) = no_call)"
         
assumes c0_freshA:

        "distinct [c0]"
         
assumes calls:

        "(calls2 = (calls1(c0 := (message_author_assign m from_init))))"
         
assumes c11_freshB:

        "((calls2 c11) = no_call)"
         
assumes c11_freshA:

        "distinct [c11 , c0]"
         
assumes calls_2:

        "(calls3 = (calls2(c11 := (message_content_assign m content_init))))"
         
assumes c21_freshB:

        "((calls3 c21) = no_call)"
         
assumes c21_freshA:

        "distinct [c21 , c0 , c11]"
         
assumes calls_3:

        "(calls4 = (calls3(c21 := (message_chat_assign m toC_init))))"
         
assumes c31_freshB:

        "((calls4 c31) = no_call)"
         
assumes c31_freshA:

        "distinct [c31 , c0 , c11 , c21]"
         
assumes calls_4:

        "(calls5 = (calls4(c31 := (chat_messages_add toC_init m))))"
         
assumes vis:

        "(vis1 = (vis \<union> {c0}))"
         
assumes vis_2:

        "(vis2 = (vis1 \<union> {c11}))"
         
assumes vis_3:

        "(vis3 = (vis2 \<union> {c21}))"
         
assumes happensBefore:

        "(happensBefore2 = (happensBefore1(c0 := (vis \<union> {c0}))))"
         
assumes happensBefore_2:

        "(happensBefore3 = (happensBefore2(c11 := (vis1 \<union> {c11}))))"
         
assumes happensBefore_3:

        "(happensBefore4 = (happensBefore3(c21 := (vis2 \<union> {c21}))))"
         
assumes happensBefore_4:

        "(happensBefore5 = (happensBefore4(c31 := (vis3 \<union> {c31}))))"
         
assumes chat_messages_contains_res_5:

        "(\<forall>bound_c65.
         (\<forall>bound_m12.
           (((chat_messages_contains_res4 bound_c65) bound_m12)
             = (\<exists>bound_c151.
             (((bound_c151 \<in> snapshotAddition1)
               \<and> (((calls5 bound_c151) = (chat_messages_add bound_c65 bound_m12))
                 \<and> (\<forall>bound_d36.
                   (((bound_d36 \<in> snapshotAddition1) \<and> ((calls5 bound_d36) = (chat_delete bound_c65)))
                     \<longrightarrow> (bound_d36 \<in> (happensBefore5 bound_c151))))))
               \<and> (\<forall>bound_c251.
                 (((bound_c251 \<in> snapshotAddition1)
                   \<and> (((calls5 bound_c251) = (chat_messages_remove bound_c65 bound_m12))
                     \<and> (\<forall>bound_d37.
                       (((bound_d37 \<in> snapshotAddition1) \<and> ((calls5 bound_d37) = (chat_delete bound_c65)))
                         \<longrightarrow> (bound_d37 \<in> (happensBefore5 bound_c251))))))
                   \<longrightarrow> (bound_c251 \<in> (happensBefore5 bound_c151)))))))))"
         
assumes message_exists_res_9:

        "(\<forall>bound_m12.
         ((message_exists_res8 bound_m12)
           = (\<exists>bound_c152.
           (((((bound_c152 \<in> snapshotAddition1)
             \<and> (\<exists>bound_args36. ((calls5 bound_c152) = (message_author_assign bound_m12 bound_args36))))
             \<or> ((bound_c152 \<in> snapshotAddition1)
               \<and> (\<exists>bound_args37. ((calls5 bound_c152) = (message_content_assign bound_m12 bound_args37)))))
             \<or> ((bound_c152 \<in> snapshotAddition1)
               \<and> (\<exists>bound_args38. ((calls5 bound_c152) = (message_chat_assign bound_m12 bound_args38)))))
             \<and> (\<forall>bound_c252.
               (((bound_c252 \<in> snapshotAddition1) \<and> ((calls5 bound_c252) = (message_delete bound_m12)))
                 \<longrightarrow> (bound_c252 \<in> (happensBefore5 bound_c152))))))))"
         
assumes vis_4:

        "(vis4 = (vis3 \<union> {c31}))"
         
assumes chat_messages_contains_res_6:

        "(\<forall>bound_c66.
         (\<forall>bound_m13.
           (((chat_messages_contains_res5 bound_c66) bound_m13)
             = (\<exists>bound_c153.
             (((bound_c153 \<in> (vis4 \<union> snapshotAddition1))
               \<and> (((calls5 bound_c153) = (chat_messages_add bound_c66 bound_m13))
                 \<and> (\<forall>bound_d38.
                   (((bound_d38 \<in> (vis4 \<union> snapshotAddition1)) \<and> ((calls5 bound_d38) = (chat_delete bound_c66)))
                     \<longrightarrow> (bound_d38 \<in> (happensBefore5 bound_c153))))))
               \<and> (\<forall>bound_c253.
                 (((bound_c253 \<in> (vis4 \<union> snapshotAddition1))
                   \<and> (((calls5 bound_c253) = (chat_messages_remove bound_c66 bound_m13))
                     \<and> (\<forall>bound_d39.
                       (((bound_d39 \<in> (vis4 \<union> snapshotAddition1)) \<and> ((calls5 bound_d39) = (chat_delete bound_c66)))
                         \<longrightarrow> (bound_d39 \<in> (happensBefore5 bound_c253))))))
                   \<longrightarrow> (bound_c253 \<in> (happensBefore5 bound_c153)))))))))"
         
assumes message_exists_res_10:

        "(\<forall>bound_m13.
         ((message_exists_res9 bound_m13)
           = (\<exists>bound_c154.
           (((((bound_c154 \<in> (vis4 \<union> snapshotAddition1))
             \<and> (\<exists>bound_args39. ((calls5 bound_c154) = (message_author_assign bound_m13 bound_args39))))
             \<or> ((bound_c154 \<in> (vis4 \<union> snapshotAddition1))
               \<and> (\<exists>bound_args40. ((calls5 bound_c154) = (message_content_assign bound_m13 bound_args40)))))
             \<or> ((bound_c154 \<in> (vis4 \<union> snapshotAddition1))
               \<and> (\<exists>bound_args41. ((calls5 bound_c154) = (message_chat_assign bound_m13 bound_args41)))))
             \<and> (\<forall>bound_c254.
               (((bound_c254 \<in> (vis4 \<union> snapshotAddition1)) \<and> ((calls5 bound_c254) = (message_delete bound_m13)))
                 \<longrightarrow> (bound_c254 \<in> (happensBefore5 bound_c154))))))))"
         
assumes invariant_not_violated:

        "\<not>((\<forall>bound_c65. (\<forall>bound_m12. (((chat_messages_contains_res4 bound_c65) bound_m12) \<longrightarrow> (message_exists_res8 bound_m12))))
         \<and> (\<forall>bound_c66. (\<forall>bound_m13. (((chat_messages_contains_res5 bound_c66) bound_m13) \<longrightarrow> (message_exists_res9 bound_m13)))))"
         shows False
  using invariant_not_violated
proof (rule notE)
  show "(\<forall>bound_c65 bound_m12. chat_messages_contains_res4 bound_c65 bound_m12 \<longrightarrow> message_exists_res8 bound_m12) \<and>
    (\<forall>bound_c66 bound_m13. chat_messages_contains_res5 bound_c66 bound_m13 \<longrightarrow> message_exists_res9 bound_m13)"
  proof (intro allI conjI impI)
  fix c m
  assume a1: "chat_messages_contains_res4 c m"
  have "c0 \<notin> snapshotAddition"
    using before_procedure_invocation_snapshot_addition_subset_calls c0_freshB growth_calls by auto

  have "c21 \<notin> snapshotAddition"
    by (metis before_procedure_invocation_snapshot_addition_subset_calls c0_freshB c21_freshB calls calls_2 fun_upd_apply growth_calls m_new_id_fresh transaction_begin_message_content_assign_call_parameter_key_generated)

  have "c31 \<notin> snapshotAddition"
    by (metis before_procedure_invocation_snapshot_addition_subset_calls c0_freshB c11_freshB c31_freshB calls calls_2 calls_3 fun_upd_apply growth_calls m_new_id_fresh transaction_begin_message_chat_assign_call_parameter_key_generated)


  from a1 have "chat_messages_contains_res2 c m"
    apply (auto simp add: chat_messages_contains_res_5 chat_messages_contains_res_3)
    apply (auto simp add: calls_4 calls_3 calls_2 calls)
    find_theorems calls2

  have "chat_messages_contains_res2 c m \<longrightarrow> message_exists_res6 m"
    by (simp add: at_transaction_begin_invariant_0)

  show  "message_exists_res8 m"
  proof -
    
      

      find_theorems chat_messages_contains_res2
