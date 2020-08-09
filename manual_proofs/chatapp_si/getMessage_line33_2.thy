
theory "getMessage_line33_2"
  imports Main
begin

      datatype CallId = CallId nat

datatype TxId = TxId nat

datatype MessageId = MessageId nat

datatype SetOp_MessageId =
    Add_MessageId (x: "MessageId")
  | Remove_MessageId (x1: "MessageId")

datatype UserId = UserId nat

datatype RegisterOp_UserId =
    Assign_UserId (qvalue: "UserId")

datatype String = String nat

datatype RegisterOp_String =
    Assign_String (value1: "String")

datatype StructAuthorContentOp =
    author (nested1: "RegisterOp_UserId")
  | content (nested2: "RegisterOp_String")

datatype MapOp_MessageId_StructAuthorContentOp =
    DeleteKey_MessageId_StructAuthorContentOp (key: "MessageId")
  | NestedOp_MessageId_StructAuthorContentOp (key1: "MessageId") (op: "StructAuthorContentOp")

datatype rootCrdtOp =
    chat (nested: "SetOp_MessageId")
  | message (nested3: "MapOp_MessageId_StructAuthorContentOp")

datatype SetQuery_MessageId =
    Contains_MessageId (x2: "MessageId")

datatype RegisterQry_UserId =
    ReadRegister_UserId

datatype MVRegisterQry_String =
    ReadFirst_String
  | MvContains_String (value2: "String")

datatype StructAuthorContentQuery =
    authorQry (nested5: "RegisterQry_UserId")
  | contentQry (nested6: "MVRegisterQry_String")

datatype MapQuery_MessageId_StructAuthorContentQuery =
    ContainsKey_MessageId_StructAuthorContentQuery (key2: "MessageId")
  | NestedQuery_MessageId_StructAuthorContentQuery (key3: "MessageId") (q: "StructAuthorContentQuery")

datatype rootCrdtQuery =
    chatQry (nested4: "SetQuery_MessageId")
  | messageQry (nested7: "MapQuery_MessageId_StructAuthorContentQuery")

datatype callInfo =
    Op (operation: "rootCrdtOp")
  | NoCall
  | Qry (query: "rootCrdtQuery")

datatype InvocationId = InvocationId nat

datatype getMessageResult =
    notFound
  | found (getMessageResult_author: "UserId") (getMessageResult_content: "String")

datatype invocationResult =
    getMessage_res (getMessage_res_arg: "getMessageResult")
  | deleteMessage_res
  | sendMessage_res (sendMessage_res_arg: "MessageId")
  | editMessage_res
  | NoResult

datatype invocationInfo =
    sendMessage (qfrom: "UserId") (qtext: "String")
  | getMessage (m: "MessageId")
  | editMessage (id: "MessageId") (newContent: "String")
  | deleteMessage (message_id: "MessageId")
  | no_invocation

lemma "getMessage_line33":
fixes vis :: "CallId set"

fixes bound_tx92 :: "TxId"

fixes bound_tx16 :: "TxId"

fixes happensBefore3 :: "CallId => CallId set"

fixes bound_tx12 :: "TxId"

fixes bound_tx43 :: "TxId"

fixes bound_tx60 :: "TxId"

fixes bound_tx71 :: "TxId"

fixes bound_tx18 :: "TxId"

fixes transactionOrigin1 :: "TxId => InvocationId option"

fixes knownIds_MessageId1 :: "MessageId set"

fixes bound_tx40 :: "TxId"

fixes bound_tx87 :: "TxId"

fixes bound_tx39 :: "TxId"

fixes bound_tx82 :: "TxId"

fixes bound_tx73 :: "TxId"

fixes bound_tx29 :: "TxId"

fixes bound_tx23 :: "TxId"

fixes bound_tx84 :: "TxId"

fixes c11 :: "CallId"

fixes tx1 :: "TxId"

fixes invocationRes :: "InvocationId => invocationResult"

fixes bound_tx65 :: "TxId"

fixes q__query_messageQry_NestedQuery_authorQry_ReadRegister_res_2 :: "UserId"

fixes bound_tx22 :: "TxId"

fixes bound_tx76 :: "TxId"

fixes bound_tx90 :: "TxId"

fixes vis1 :: "CallId set"

fixes q__query_messageQry_NestedQuery_contentQry_ReadFirst_res_3 :: "String"

fixes bound_tx33 :: "TxId"

fixes bound_tx69 :: "TxId"

fixes messageQry_ContainsKey_res :: "bool"

fixes calls1 :: "CallId => callInfo"

fixes bound_tx44 :: "TxId"

fixes invocationOp :: "InvocationId => invocationInfo"

fixes callOrigin :: "CallId => TxId option"

fixes bound_tx32 :: "TxId"

fixes bound_tx77 :: "TxId"

fixes bound_tx81 :: "TxId"

fixes invocationRes1 :: "InvocationId => invocationResult"

fixes invocationCalls :: "InvocationId => CallId set"

fixes calls :: "CallId => callInfo"

fixes bound_tx36 :: "TxId"

fixes calls3 :: "CallId => callInfo"

fixes bound_tx70 :: "TxId"

fixes bound_tx15 :: "TxId"

fixes newCalls :: "CallId set"

fixes bound_tx66 :: "TxId"

fixes bound_tx26 :: "TxId"

fixes g1 :: "InvocationId"

fixes bound_tx19 :: "TxId"

fixes bound_tx37 :: "TxId"

fixes q__query_messageQry_ContainsKey_res_1 :: "bool"

fixes bound_tx88 :: "TxId"

fixes vis2 :: "CallId set"

fixes bound_tx62 :: "TxId"

fixes transactionOrigin :: "TxId => InvocationId option"

fixes callOrigin1 :: "CallId => TxId option"

fixes bound_tx35 :: "TxId"

fixes calls2 :: "CallId => callInfo"

fixes bound_tx63 :: "TxId"

fixes c21 :: "CallId"

fixes bound_tx85 :: "TxId"

fixes bound_tx27 :: "TxId"

fixes bound_tx25 :: "TxId"

fixes snapshotAddition :: "CallId set"

fixes bound_tx89 :: "TxId"

fixes happensBefore1 :: "CallId => CallId set"

fixes bound_tx14 :: "TxId"

fixes bound_tx41 :: "TxId"

fixes author2 :: "UserId"

fixes messageQry_NestedQuery_authorQry_ReadRegister :: "UserId"

fixes content2 :: "String"

fixes generatedIds_MessageId1 :: "MessageId => InvocationId option"

fixes generatedIds_MessageId :: "MessageId => InvocationId option"

fixes m_init :: "MessageId"

fixes invocationOp1 :: "InvocationId => invocationInfo"

fixes bound_tx78 :: "TxId"

fixes bound_tx31 :: "TxId"

fixes bound_tx67 :: "TxId"

fixes bound_tx21 :: "TxId"

fixes snapshotAddition1 :: "CallId set"

fixes currentInvocation :: "InvocationId"

fixes bound_tx74 :: "TxId"

fixes query_messageQry_NestedQuery_authorQry_ReadRegister_postcondition :: "bool"

fixes bound_tx17 :: "TxId"

fixes c0 :: "CallId"

fixes bound_tx61 :: "TxId"

fixes bound_tx24 :: "TxId"

fixes invocationCalls1 :: "InvocationId => CallId set"

fixes bound_tx42 :: "TxId"

fixes query_messageQry_NestedQuery_contentQry_ReadFirst_postcondition :: "bool"

fixes bound_tx38 :: "TxId"

fixes bound_tx91 :: "TxId"

fixes knownIds_MessageId :: "MessageId set"

fixes bound_tx13 :: "TxId"

fixes happensBefore2 :: "CallId => CallId set"

fixes bound_tx28 :: "TxId"

fixes bound_tx83 :: "TxId"

fixes m3 :: "MessageId"

fixes bound_tx34 :: "TxId"

fixes bound_tx86 :: "TxId"

fixes bound_tx72 :: "TxId"

fixes bound_tx75 :: "TxId"

fixes bound_tx64 :: "TxId"

fixes bound_tx80 :: "TxId"

fixes bound_tx30 :: "TxId"

fixes bound_tx79 :: "TxId"

fixes bound_tx68 :: "TxId"

fixes bound_tx20 :: "TxId"

fixes messageQry_NestedQuery_contentQry_ReadFirst :: "String"

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

        "(\<forall>bound_c39. ((bound_c39 \<in> snapshotAddition) \<longrightarrow> ((calls bound_c39) \<noteq> NoCall)))"

assumes before_procedure_invocation_MessageId_knownIds_are_generated:

        "(\<forall>bound_x11. ((bound_x11 \<in> knownIds_MessageId) \<longrightarrow> \<not>((generatedIds_MessageId bound_x11) = None)))"

assumes before_procedure_invocation_call_parameters_generated:

        "(\<forall>bound_c38.
         (\<forall>bound_uid.
           ((bound_uid \<in> (uniqueIds_op_MessageId_SortCall_1 (calls bound_c38))) \<longrightarrow> ((generatedIds_MessageId bound_uid) \<noteq> None))))"

assumes before_procedure_invocation_database_calls_MessageId_are_generated:

        "(\<forall>bound_c37.
         (\<forall>bound_x10.
           ((bound_x10 \<in> (uniqueIds_op_MessageId_SortCall_1 (calls bound_c37))) \<longrightarrow> ((generatedIds_MessageId bound_x10) \<noteq> None))))"

assumes before_procedure_invocation_getMessage_MessageId_res_known:

        "(\<forall>bound_i11. ((uniqueIds_op_MessageId_SortInvocationRes_1 (invocationRes bound_i11)) \<subseteq> knownIds_MessageId))"

assumes before_procedure_invocation_getMessage_MessageId_args_known:

        "(\<forall>bound_i10. ((uniqueIds_op_MessageId_SortInvocationInfo_1 (invocationOp bound_i10)) \<subseteq> knownIds_MessageId))"

assumes before_procedure_invocation_deleteMessage_MessageId_res_known:

        "(\<forall>bound_i9. ((uniqueIds_op_MessageId_SortInvocationRes_1 (invocationRes bound_i9)) \<subseteq> knownIds_MessageId))"

assumes before_procedure_invocation_deleteMessage_MessageId_args_known:

        "(\<forall>bound_i8. ((uniqueIds_op_MessageId_SortInvocationInfo_1 (invocationOp bound_i8)) \<subseteq> knownIds_MessageId))"

assumes before_procedure_invocation_editMessage_MessageId_res_known:

        "(\<forall>bound_i7. ((uniqueIds_op_MessageId_SortInvocationRes_1 (invocationRes bound_i7)) \<subseteq> knownIds_MessageId))"

assumes before_procedure_invocation_editMessage_MessageId_args_known:

        "(\<forall>bound_i6. ((uniqueIds_op_MessageId_SortInvocationInfo_1 (invocationOp bound_i6)) \<subseteq> knownIds_MessageId))"

assumes before_procedure_invocation_sendMessage_MessageId_res_known:

        "(\<forall>bound_i5. ((uniqueIds_op_MessageId_SortInvocationRes_1 (invocationRes bound_i5)) \<subseteq> knownIds_MessageId))"

assumes before_procedure_invocation_sendMessage_MessageId_args_known:

        "(\<forall>bound_i4. ((uniqueIds_op_MessageId_SortInvocationInfo_1 (invocationOp bound_i4)) \<subseteq> knownIds_MessageId))"

assumes before_procedure_invocation_WF_transactionOrigin_exists:

        "(\<forall>bound_tx48.
         (\<forall>bound_i3. (((transactionOrigin bound_tx48) = (Some bound_i3)) \<longrightarrow> ((invocationOp bound_i3) \<noteq> no_invocation))))"

assumes before_procedure_invocation_WF_callOrigin_exists:

        "(\<forall>bound_ca. (\<forall>bound_tx47. (((callOrigin bound_ca) = (Some bound_tx47)) \<longrightarrow> \<not>((transactionOrigin bound_tx47) = None))))"

assumes before_procedure_invocation_WF_no_call_implies_no_happensBefore:

        "(\<forall>bound_c36. (((calls bound_c36) = NoCall) \<longrightarrow> ((happensBefore bound_c36) = {})))"

assumes before_procedure_invocation_WF_transactionOrigin_callOrigin:

        "(\<forall>bound_tx46. (((transactionOrigin bound_tx46) = None) \<longrightarrow> (\<forall>bound_c35. ((callOrigin bound_c35) \<noteq> (Some bound_tx46)))))"

assumes before_procedure_invocation_WF_callOrigin:

        "(\<forall>bound_c34. (((callOrigin bound_c34) = None) = ((calls bound_c34) = NoCall)))"

assumes before_procedure_invocation_WF_invocationCalls:

        "(\<forall>bound_i2.
         (\<forall>bound_c33.
           ((bound_c33 \<in> (invocationCalls bound_i2))
             = (\<exists>bound_tx45.
             (((callOrigin bound_c33) = (Some bound_tx45)) \<and> ((transactionOrigin bound_tx45) = (Some bound_i2)))))))"

assumes before_procedure_invocation_happens_before_transaction_consistent_r:

        "(\<forall>bound_x9.
         (\<forall>bound_y12.
           (\<forall>bound_y22.
             (((((callOrigin bound_y12) = (callOrigin bound_y22)) \<and> \<not>((callOrigin bound_x9) = (callOrigin bound_y12)))
               \<and> (bound_x9 \<in> (happensBefore bound_y12)))
               \<longrightarrow> (bound_x9 \<in> (happensBefore bound_y22))))))"

assumes before_procedure_invocation_happens_before_transaction_consistent_l:

        "(\<forall>bound_x8.
         (\<forall>bound_y11.
           (\<forall>bound_y21.
             (((((callOrigin bound_y11) = (callOrigin bound_y21)) \<and> \<not>((callOrigin bound_x8) = (callOrigin bound_y11)))
               \<and> (bound_y11 \<in> (happensBefore bound_x8)))
               \<longrightarrow> (bound_y21 \<in> (happensBefore bound_x8))))))"

assumes before_procedure_invocation_no_invocation_implies_no_result:

        "(\<forall>bound_i1. (((invocationOp bound_i1) = no_invocation) \<longrightarrow> ((invocationRes bound_i1) = NoResult)))"

assumes before_procedure_invocation_happensBefore_antisym:

        "(\<forall>bound_x7. (\<forall>bound_y1. ((bound_x7 \<in> (happensBefore bound_y1)) \<longrightarrow> \<not>(bound_y1 \<in> (happensBefore bound_x7)))))"

assumes before_procedure_invocation_happensBefore_trans:

        "(\<forall>bound_x6.
         (\<forall>bound_y.
           (\<forall>bound_z.
             (((bound_x6 \<in> (happensBefore bound_y)) \<and> (bound_y \<in> (happensBefore bound_z)))
               \<longrightarrow> (bound_x6 \<in> (happensBefore bound_z))))))"

assumes before_procedure_invocation_happensBefore_non_reflex:

        "(\<forall>bound_c32. \<not>(bound_c32 \<in> (happensBefore bound_c32)))"

assumes before_procedure_invocation_visibleCalls_causally_consistent:

        "(\<forall>bound_c114. (\<forall>bound_c214. (((bound_c214 \<in> {}) \<and> (bound_c114 \<in> (happensBefore bound_c214))) \<longrightarrow> (bound_c114 \<in> {}))))"

assumes before_procedure_invocation_visibleCalls_transaction_consistent1:

        "(\<forall>bound_c113.
         (\<forall>bound_c213.
           ((((bound_c113 \<in> {}) \<and> ((callOrigin bound_c113) = (callOrigin bound_c213))) \<and> ((calls bound_c213) \<noteq> NoCall))
             \<longrightarrow> (bound_c213 \<in> {}))))"

assumes before_procedure_invocation_visibleCalls_exist:

        "(\<forall>bound_c31. ((bound_c31 \<in> {}) \<longrightarrow> ((calls bound_c31) \<noteq> NoCall)))"

assumes before_procedure_invocation_invocation_sequential:

        "(\<forall>bound_c112.
         (\<forall>bound_tx110.
           (\<forall>bound_i.
             (\<forall>bound_c212.
               (\<forall>bound_tx210.
                 (((((((callOrigin bound_c112) = (Some bound_tx110)) \<and> ((transactionOrigin bound_tx110) = (Some bound_i)))
                   \<and> ((callOrigin bound_c212) = (Some bound_tx210)))
                   \<and> ((transactionOrigin bound_tx210) = (Some bound_i)))
                   \<and> (bound_c112 \<noteq> bound_c212))
                   \<longrightarrow> ((bound_c112 \<in> (happensBefore bound_c212)) \<or> (bound_c212 \<in> (happensBefore bound_c112)))))))))"

assumes before_procedure_invocation_happensBefore_exists_r:

        "(\<forall>bound_c111. (\<forall>bound_c211. ((bound_c111 \<in> (happensBefore bound_c211)) \<longrightarrow> ((calls bound_c211) \<noteq> NoCall))))"

assumes before_procedure_invocation_happensBefore_exists_l:

        "(\<forall>bound_c110. (\<forall>bound_c210. ((bound_c110 \<in> (happensBefore bound_c210)) \<longrightarrow> ((calls bound_c110) \<noteq> NoCall))))"

assumes no_call_in_new_invocation:

        "((invocationCalls currentInvocation) = {})"

assumes no_transaction_in_new_invocation:

        "(\<forall>tx. ((transactionOrigin tx) \<noteq> (Some currentInvocation)))"

assumes before_procedure_invocation_invariant_shape_rev_ShapeCall_Qry_messageQry_NestedQuery_MessageId_StructAuthorContentQuery_x_19_contentQry_MvContains_String_x_22_q:

        "(\<forall>bound_c30.
         (\<forall>bound_arg_0_x_195.
           (\<forall>bound_arg_1_x_22.
             \<not>((calls bound_c30)
               = (Qry (messageQry (NestedQuery_MessageId_StructAuthorContentQuery bound_arg_0_x_195
                     (contentQry (MvContains_String bound_arg_1_x_22)))))))))"

assumes before_procedure_invocation_invariant_shape_rev_ShapeCall_Qry_messageQry_NestedQuery_MessageId_StructAuthorContentQuery_x_19_contentQry_ReadFirst_String_q:

        "(\<forall>bound_c29.
         (\<forall>bound_arg_0_x_194.
           (((calls bound_c29)
             = (Qry (messageQry (NestedQuery_MessageId_StructAuthorContentQuery bound_arg_0_x_194
                   (contentQry (ReadFirst_String ))))))
             \<longrightarrow> (\<exists>bound_invoc36.
               (((case (callOrigin bound_c29) of    Some bound_tx44 => (transactionOrigin bound_tx44) | None => None)
                 = (Some bound_invoc36))
                 \<and> ((invocationOp bound_invoc36) = (getMessage bound_arg_0_x_194)))))))"

assumes before_procedure_invocation_invariant_shape_rev_ShapeCall_Qry_messageQry_NestedQuery_MessageId_StructAuthorContentQuery_x_19_contentQry_x_21_q:

        "(\<forall>bound_c28.
         (\<forall>bound_arg_0_x_193.
           (\<forall>bound_arg_1_x_21.
             (((calls bound_c28)
               = (Qry (messageQry (NestedQuery_MessageId_StructAuthorContentQuery bound_arg_0_x_193
                     (contentQry bound_arg_1_x_21)))))
               \<longrightarrow> (\<exists>bound_invoc35.
                 (((case (callOrigin bound_c28) of    Some bound_tx43 => (transactionOrigin bound_tx43) | None => None)
                   = (Some bound_invoc35))
                   \<and> ((invocationOp bound_invoc35) = (getMessage bound_arg_0_x_193))))))))"

assumes before_procedure_invocation_invariant_shape_rev_ShapeCall_Qry_messageQry_NestedQuery_MessageId_StructAuthorContentQuery_x_19_authorQry_ReadRegister_UserId_q:

        "(\<forall>bound_c27.
         (\<forall>bound_arg_0_x_192.
           (((calls bound_c27)
             = (Qry (messageQry (NestedQuery_MessageId_StructAuthorContentQuery bound_arg_0_x_192
                   (authorQry (ReadRegister_UserId ))))))
             \<longrightarrow> (\<exists>bound_invoc34.
               (((case (callOrigin bound_c27) of    Some bound_tx42 => (transactionOrigin bound_tx42) | None => None)
                 = (Some bound_invoc34))
                 \<and> ((invocationOp bound_invoc34) = (getMessage bound_arg_0_x_192)))))))"

assumes before_procedure_invocation_invariant_shape_rev_ShapeCall_Qry_messageQry_NestedQuery_MessageId_StructAuthorContentQuery_x_19_authorQry_x_20_q:

        "(\<forall>bound_c26.
         (\<forall>bound_arg_0_x_191.
           (\<forall>bound_arg_1_x_20.
             (((calls bound_c26)
               = (Qry (messageQry (NestedQuery_MessageId_StructAuthorContentQuery bound_arg_0_x_191
                     (authorQry bound_arg_1_x_20)))))
               \<longrightarrow> (\<exists>bound_invoc33.
                 (((case (callOrigin bound_c26) of    Some bound_tx41 => (transactionOrigin bound_tx41) | None => None)
                   = (Some bound_invoc33))
                   \<and> ((invocationOp bound_invoc33) = (getMessage bound_arg_0_x_191))))))))"

assumes before_procedure_invocation_invariant_shape_rev_ShapeCall_Qry_messageQry_NestedQuery_MessageId_StructAuthorContentQuery_x_19_x_18_q:

        "(\<forall>bound_c25.
         (\<forall>bound_arg_0_x_19.
           (\<forall>bound_arg_1_x_18.
             (((calls bound_c25)
               = (Qry (messageQry (NestedQuery_MessageId_StructAuthorContentQuery bound_arg_0_x_19 bound_arg_1_x_18))))
               \<longrightarrow> (\<exists>bound_invoc32.
                 (((case (callOrigin bound_c25) of    Some bound_tx40 => (transactionOrigin bound_tx40) | None => None)
                   = (Some bound_invoc32))
                   \<and> ((invocationOp bound_invoc32) = (getMessage bound_arg_0_x_19))))))))"

assumes before_procedure_invocation_invariant_shape_rev_ShapeCall_Qry_messageQry_ContainsKey_MessageId_StructAuthorContentQuery_x_17_q:

        "(\<forall>bound_c24.
         (\<forall>bound_arg_0_x_17.
           (((calls bound_c24) = (Qry (messageQry (ContainsKey_MessageId_StructAuthorContentQuery bound_arg_0_x_17))))
             \<longrightarrow> (((\<exists>bound_invoc29.
               (\<exists>bound_newContent7.
                 (((case (callOrigin bound_c24) of    Some bound_tx37 => (transactionOrigin bound_tx37) | None => None)
                   = (Some bound_invoc29))
                   \<and> ((invocationOp bound_invoc29) = (editMessage bound_arg_0_x_17 bound_newContent7)))))
               \<or> (\<exists>bound_invoc30.
                 (((case (callOrigin bound_c24) of    Some bound_tx38 => (transactionOrigin bound_tx38) | None => None)
                   = (Some bound_invoc30))
                   \<and> ((invocationOp bound_invoc30) = (deleteMessage bound_arg_0_x_17)))))
               \<or> (\<exists>bound_invoc31.
                 (((case (callOrigin bound_c24) of    Some bound_tx39 => (transactionOrigin bound_tx39) | None => None)
                   = (Some bound_invoc31))
                   \<and> ((invocationOp bound_invoc31) = (getMessage bound_arg_0_x_17))))))))"

assumes before_procedure_invocation_invariant_shape_rev_ShapeCall_Qry_messageQry_x_16_q:

        "(\<forall>bound_c23.
         (\<forall>bound_arg_0_x_16.
           (((calls bound_c23) = (Qry (messageQry bound_arg_0_x_16)))
             \<longrightarrow> (((\<exists>bound_invoc26.
               (\<exists>bound_id4.
                 (\<exists>bound_newContent6.
                   (((case (callOrigin bound_c23) of    Some bound_tx34 => (transactionOrigin bound_tx34) | None => None)
                     = (Some bound_invoc26))
                     \<and> ((invocationOp bound_invoc26) = (editMessage bound_id4 bound_newContent6))))))
               \<or> (\<exists>bound_invoc27.
                 (\<exists>bound_message_id5.
                   (((case (callOrigin bound_c23) of    Some bound_tx35 => (transactionOrigin bound_tx35) | None => None)
                     = (Some bound_invoc27))
                     \<and> ((invocationOp bound_invoc27) = (deleteMessage bound_message_id5))))))
               \<or> (\<exists>bound_invoc28.
                 (\<exists>bound_m5.
                   (((case (callOrigin bound_c23) of    Some bound_tx36 => (transactionOrigin bound_tx36) | None => None)
                     = (Some bound_invoc28))
                     \<and> ((invocationOp bound_invoc28) = (getMessage bound_m5)))))))))"

assumes before_procedure_invocation_invariant_shape_rev_ShapeCall_Qry_chatQry_Contains_MessageId_x_15_q:

        "(\<forall>bound_c22. (\<forall>bound_arg_0_x_15. \<not>((calls bound_c22) = (Qry (chatQry (Contains_MessageId bound_arg_0_x_15))))))"

assumes before_procedure_invocation_invariant_shape_rev_ShapeCall_Qry_chatQry_x_14_q:

        "(\<forall>bound_c21. (\<forall>bound_arg_0_x_14. \<not>((calls bound_c21) = (Qry (chatQry bound_arg_0_x_14)))))"

assumes before_procedure_invocation_invariant_shape_rev_ShapeCall_Qry_x_13_q:

        "(\<forall>bound_c20.
         (\<forall>bound_arg_0_x_13.
           (((calls bound_c20) = (Qry bound_arg_0_x_13))
             \<longrightarrow> (((\<exists>bound_invoc23.
               (\<exists>bound_id3.
                 (\<exists>bound_newContent5.
                   (((case (callOrigin bound_c20) of    Some bound_tx31 => (transactionOrigin bound_tx31) | None => None)
                     = (Some bound_invoc23))
                     \<and> ((invocationOp bound_invoc23) = (editMessage bound_id3 bound_newContent5))))))
               \<or> (\<exists>bound_invoc24.
                 (\<exists>bound_message_id4.
                   (((case (callOrigin bound_c20) of    Some bound_tx32 => (transactionOrigin bound_tx32) | None => None)
                     = (Some bound_invoc24))
                     \<and> ((invocationOp bound_invoc24) = (deleteMessage bound_message_id4))))))
               \<or> (\<exists>bound_invoc25.
                 (\<exists>bound_m4.
                   (((case (callOrigin bound_c20) of    Some bound_tx33 => (transactionOrigin bound_tx33) | None => None)
                     = (Some bound_invoc25))
                     \<and> ((invocationOp bound_invoc25) = (getMessage bound_m4)))))))))"

assumes before_procedure_invocation_invariant_shape_rev_ShapeCall_Op_message_NestedOp_MessageId_StructAuthorContentOp_x_8_content_Assign_String_x_12_q:

        "(\<forall>bound_c19.
         (\<forall>bound_arg_0_x_84.
           (\<forall>bound_arg_1_x_12.
             (((calls bound_c19)
               = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_arg_0_x_84
                     (content (Assign_String bound_arg_1_x_12))))))
               \<longrightarrow> ((\<exists>bound_invoc21.
                 (\<exists>bound_from8.
                   (((case (callOrigin bound_c19) of    Some bound_tx29 => (transactionOrigin bound_tx29) | None => None)
                     = (Some bound_invoc21))
                     \<and> ((invocationOp bound_invoc21) = (sendMessage bound_from8 bound_arg_1_x_12)))))
                 \<or> (\<exists>bound_invoc22.
                   (((case (callOrigin bound_c19) of    Some bound_tx30 => (transactionOrigin bound_tx30) | None => None)
                     = (Some bound_invoc22))
                     \<and> ((invocationOp bound_invoc22) = (editMessage bound_arg_0_x_84 bound_arg_1_x_12)))))))))"

assumes before_procedure_invocation_invariant_shape_rev_ShapeCall_Op_message_NestedOp_MessageId_StructAuthorContentOp_x_8_content_x_11_q:

        "(\<forall>bound_c18.
         (\<forall>bound_arg_0_x_83.
           (\<forall>bound_arg_1_x_11.
             (((calls bound_c18)
               = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_arg_0_x_83 (content bound_arg_1_x_11)))))
               \<longrightarrow> ((\<exists>bound_invoc19.
                 (\<exists>bound_from7.
                   (\<exists>bound_text8.
                     (((case (callOrigin bound_c18) of    Some bound_tx27 => (transactionOrigin bound_tx27) | None => None)
                       = (Some bound_invoc19))
                       \<and> ((invocationOp bound_invoc19) = (sendMessage bound_from7 bound_text8))))))
                 \<or> (\<exists>bound_invoc20.
                   (\<exists>bound_newContent4.
                     (((case (callOrigin bound_c18) of    Some bound_tx28 => (transactionOrigin bound_tx28) | None => None)
                       = (Some bound_invoc20))
                       \<and> ((invocationOp bound_invoc20) = (editMessage bound_arg_0_x_83 bound_newContent4))))))))))"

assumes before_procedure_invocation_invariant_shape_rev_ShapeCall_Op_message_NestedOp_MessageId_StructAuthorContentOp_x_8_author_Assign_UserId_x_10_q:

        "(\<forall>bound_c17.
         (\<forall>bound_arg_0_x_82.
           (\<forall>bound_arg_1_x_10.
             (((calls bound_c17)
               = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_arg_0_x_82
                     (author (Assign_UserId bound_arg_1_x_10))))))
               \<longrightarrow> (\<exists>bound_invoc18.
                 (\<exists>bound_text7.
                   (((case (callOrigin bound_c17) of    Some bound_tx26 => (transactionOrigin bound_tx26) | None => None)
                     = (Some bound_invoc18))
                     \<and> ((invocationOp bound_invoc18) = (sendMessage bound_arg_1_x_10 bound_text7)))))))))"

assumes before_procedure_invocation_invariant_shape_rev_ShapeCall_Op_message_NestedOp_MessageId_StructAuthorContentOp_x_8_author_x_9_q:

        "(\<forall>bound_c16.
         (\<forall>bound_arg_0_x_81.
           (\<forall>bound_arg_1_x_9.
             (((calls bound_c16)
               = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_arg_0_x_81 (author bound_arg_1_x_9)))))
               \<longrightarrow> (\<exists>bound_invoc17.
                 (\<exists>bound_from6.
                   (\<exists>bound_text6.
                     (((case (callOrigin bound_c16) of    Some bound_tx25 => (transactionOrigin bound_tx25) | None => None)
                       = (Some bound_invoc17))
                       \<and> ((invocationOp bound_invoc17) = (sendMessage bound_from6 bound_text6))))))))))"

assumes before_procedure_invocation_invariant_shape_rev_ShapeCall_Op_message_NestedOp_MessageId_StructAuthorContentOp_x_8_x_7_q:

        "(\<forall>bound_c15.
         (\<forall>bound_arg_0_x_8.
           (\<forall>bound_arg_1_x_7.
             (((calls bound_c15) = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_arg_0_x_8 bound_arg_1_x_7))))
               \<longrightarrow> ((\<exists>bound_invoc15.
                 (\<exists>bound_from5.
                   (\<exists>bound_text5.
                     (((case (callOrigin bound_c15) of    Some bound_tx23 => (transactionOrigin bound_tx23) | None => None)
                       = (Some bound_invoc15))
                       \<and> ((invocationOp bound_invoc15) = (sendMessage bound_from5 bound_text5))))))
                 \<or> (\<exists>bound_invoc16.
                   (\<exists>bound_newContent3.
                     (((case (callOrigin bound_c15) of    Some bound_tx24 => (transactionOrigin bound_tx24) | None => None)
                       = (Some bound_invoc16))
                       \<and> ((invocationOp bound_invoc16) = (editMessage bound_arg_0_x_8 bound_newContent3))))))))))"

assumes before_procedure_invocation_invariant_shape_rev_ShapeCall_Op_message_DeleteKey_MessageId_StructAuthorContentOp_x_6_q:

        "(\<forall>bound_c14.
         (\<forall>bound_arg_0_x_6.
           (((calls bound_c14) = (Op (message (DeleteKey_MessageId_StructAuthorContentOp bound_arg_0_x_6))))
             \<longrightarrow> (\<exists>bound_invoc14.
               (((case (callOrigin bound_c14) of    Some bound_tx22 => (transactionOrigin bound_tx22) | None => None)
                 = (Some bound_invoc14))
                 \<and> ((invocationOp bound_invoc14) = (deleteMessage bound_arg_0_x_6)))))))"

assumes before_procedure_invocation_invariant_shape_rev_ShapeCall_Op_message_x_5_q:

        "(\<forall>bound_c13.
         (\<forall>bound_arg_0_x_5.
           (((calls bound_c13) = (Op (message bound_arg_0_x_5)))
             \<longrightarrow> (((\<exists>bound_invoc11.
               (\<exists>bound_from4.
                 (\<exists>bound_text4.
                   (((case (callOrigin bound_c13) of    Some bound_tx19 => (transactionOrigin bound_tx19) | None => None)
                     = (Some bound_invoc11))
                     \<and> ((invocationOp bound_invoc11) = (sendMessage bound_from4 bound_text4))))))
               \<or> (\<exists>bound_invoc12.
                 (\<exists>bound_id2.
                   (\<exists>bound_newContent2.
                     (((case (callOrigin bound_c13) of    Some bound_tx20 => (transactionOrigin bound_tx20) | None => None)
                       = (Some bound_invoc12))
                       \<and> ((invocationOp bound_invoc12) = (editMessage bound_id2 bound_newContent2)))))))
               \<or> (\<exists>bound_invoc13.
                 (\<exists>bound_message_id3.
                   (((case (callOrigin bound_c13) of    Some bound_tx21 => (transactionOrigin bound_tx21) | None => None)
                     = (Some bound_invoc13))
                     \<and> ((invocationOp bound_invoc13) = (deleteMessage bound_message_id3)))))))))"

assumes before_procedure_invocation_invariant_shape_rev_ShapeCall_Op_chat_Remove_MessageId_x_4_q:

        "(\<forall>bound_c12.
         (\<forall>bound_arg_0_x_4.
           (((calls bound_c12) = (Op (chat (Remove_MessageId bound_arg_0_x_4))))
             \<longrightarrow> (\<exists>bound_invoc10.
               (((case (callOrigin bound_c12) of    Some bound_tx18 => (transactionOrigin bound_tx18) | None => None)
                 = (Some bound_invoc10))
                 \<and> ((invocationOp bound_invoc10) = (deleteMessage bound_arg_0_x_4)))))))"
assumes before_procedure_invocation_invariant_shape_rev_ShapeCall_Op_chat_Add_MessageId_x_3_q:

        "(\<forall>bound_c11.
         (\<forall>bound_arg_0_x_3.
           (((calls bound_c11) = (Op (chat (Add_MessageId bound_arg_0_x_3))))
             \<longrightarrow> (\<exists>bound_invoc9.
               (\<exists>bound_from3.
                 (\<exists>bound_text3.
                   (((case (callOrigin bound_c11) of    Some bound_tx17 => (transactionOrigin bound_tx17) | None => None)
                     = (Some bound_invoc9))
                     \<and> ((invocationOp bound_invoc9) = (sendMessage bound_from3 bound_text3)))))))))"

assumes before_procedure_invocation_invariant_shape_rev_ShapeCall_Op_chat_x_2_q:

        "(\<forall>bound_c10.
         (\<forall>bound_arg_0_x_2.
           (((calls bound_c10) = (Op (chat bound_arg_0_x_2)))
             \<longrightarrow> ((\<exists>bound_invoc7.
               (\<exists>bound_from2.
                 (\<exists>bound_text2.
                   (((case (callOrigin bound_c10) of    Some bound_tx15 => (transactionOrigin bound_tx15) | None => None)
                     = (Some bound_invoc7))
                     \<and> ((invocationOp bound_invoc7) = (sendMessage bound_from2 bound_text2))))))
               \<or> (\<exists>bound_invoc8.
                 (\<exists>bound_message_id2.
                   (((case (callOrigin bound_c10) of    Some bound_tx16 => (transactionOrigin bound_tx16) | None => None)
                     = (Some bound_invoc8))
                     \<and> ((invocationOp bound_invoc8) = (deleteMessage bound_message_id2)))))))))"

assumes before_procedure_invocation_invariant_shape_rev_ShapeCall_Op_x_1_q:

        "(\<forall>bound_c9.
         (\<forall>bound_arg_0_x_1.
           (((calls bound_c9) = (Op bound_arg_0_x_1))
             \<longrightarrow> (((\<exists>bound_invoc4.
               (\<exists>bound_from1.
                 (\<exists>bound_text1.
                   (((case (callOrigin bound_c9) of    Some bound_tx12 => (transactionOrigin bound_tx12) | None => None)
                     = (Some bound_invoc4))
                     \<and> ((invocationOp bound_invoc4) = (sendMessage bound_from1 bound_text1))))))
               \<or> (\<exists>bound_invoc5.
                 (\<exists>bound_id1.
                   (\<exists>bound_newContent1.
                     (((case (callOrigin bound_c9) of    Some bound_tx13 => (transactionOrigin bound_tx13) | None => None)
                       = (Some bound_invoc5))
                       \<and> ((invocationOp bound_invoc5) = (editMessage bound_id1 bound_newContent1)))))))
               \<or> (\<exists>bound_invoc6.
                 (\<exists>bound_message_id1.
                   (((case (callOrigin bound_c9) of    Some bound_tx14 => (transactionOrigin bound_tx14) | None => None)
                     = (Some bound_invoc6))
                     \<and> ((invocationOp bound_invoc6) = (deleteMessage bound_message_id1)))))))))"

assumes before_procedure_invocation_invariant_shape_of_invocation_getMessage:

        "(\<forall>bound_invoc3.
         (\<forall>bound_param_m.
           (((invocationOp bound_invoc3) = (getMessage bound_param_m))
             \<longrightarrow> ((((distinct [] \<and> (\<forall>bound_tx9. \<not>((transactionOrigin bound_tx9) = (Some bound_invoc3)))) \<and> distinct [])
               \<or> (\<exists>bound_tx_05.
                 ((distinct [bound_tx_05]
                   \<and> (\<forall>bound_tx10. (((transactionOrigin bound_tx10) = (Some bound_invoc3)) \<longrightarrow> (bound_tx_05 = bound_tx10))))
                   \<and> (\<exists>bound_c_05.
                     (((distinct [bound_c_05]
                       \<and> (\<forall>bound_c7. (((callOrigin bound_c7) = (Some bound_tx_05)) \<longrightarrow> (bound_c7 = bound_c_05))))
                       \<and> ((callOrigin bound_c_05) = (Some bound_tx_05)))
                       \<and> ((calls bound_c_05)
                         = (Qry (messageQry (ContainsKey_MessageId_StructAuthorContentQuery bound_param_m)))))))))
               \<or> (\<exists>bound_tx_06.
                 ((distinct [bound_tx_06]
                   \<and> (\<forall>bound_tx11. (((transactionOrigin bound_tx11) = (Some bound_invoc3)) \<longrightarrow> (bound_tx_06 = bound_tx11))))
                   \<and> (\<exists>bound_c_06.
                     (\<exists>bound_c_13.
                       (\<exists>bound_c_22.
                         (((((((((distinct [bound_c_06 , bound_c_13 , bound_c_22]
                           \<and> (\<forall>bound_c8.
                             (((callOrigin bound_c8) = (Some bound_tx_06))
                               \<longrightarrow> (((bound_c8 = bound_c_06) \<or> (bound_c8 = bound_c_13)) \<or> (bound_c8 = bound_c_22)))))
                           \<and> ((callOrigin bound_c_06) = (Some bound_tx_06)))
                           \<and> ((callOrigin bound_c_13) = (Some bound_tx_06)))
                           \<and> ((callOrigin bound_c_22) = (Some bound_tx_06)))
                           \<and> ((calls bound_c_06)
                             = (Qry (messageQry (ContainsKey_MessageId_StructAuthorContentQuery bound_param_m)))))
                           \<and> ((calls bound_c_13)
                             = (Qry (messageQry (NestedQuery_MessageId_StructAuthorContentQuery bound_param_m
                                   (authorQry (ReadRegister_UserId )))))))
                           \<and> ((calls bound_c_22)
                             = (Qry (messageQry (NestedQuery_MessageId_StructAuthorContentQuery bound_param_m
                                   (contentQry (ReadFirst_String )))))))
                           \<and> (bound_c_06 \<in> (happensBefore bound_c_13)))
                           \<and> (bound_c_13 \<in> (happensBefore bound_c_22))))))))))))"

assumes before_procedure_invocation_invariant_shape_of_invocation_deleteMessage:

        "(\<forall>bound_invoc2.
         (\<forall>bound_param_message_id.
           (((invocationOp bound_invoc2) = (deleteMessage bound_param_message_id))
             \<longrightarrow> ((((distinct [] \<and> (\<forall>bound_tx6. \<not>((transactionOrigin bound_tx6) = (Some bound_invoc2)))) \<and> distinct [])
               \<or> (\<exists>bound_tx_03.
                 ((distinct [bound_tx_03]
                   \<and> (\<forall>bound_tx7. (((transactionOrigin bound_tx7) = (Some bound_invoc2)) \<longrightarrow> (bound_tx_03 = bound_tx7))))
                   \<and> (\<exists>bound_c_03.
                     (((distinct [bound_c_03]
                       \<and> (\<forall>bound_c5. (((callOrigin bound_c5) = (Some bound_tx_03)) \<longrightarrow> (bound_c5 = bound_c_03))))
                       \<and> ((callOrigin bound_c_03) = (Some bound_tx_03)))
                       \<and> ((calls bound_c_03)
                         = (Qry (messageQry (ContainsKey_MessageId_StructAuthorContentQuery bound_param_message_id)))))))))
               \<or> (\<exists>bound_tx_04.
                 ((distinct [bound_tx_04]
                   \<and> (\<forall>bound_tx8. (((transactionOrigin bound_tx8) = (Some bound_invoc2)) \<longrightarrow> (bound_tx_04 = bound_tx8))))
                   \<and> (\<exists>bound_c_04.
                     (\<exists>bound_c_12.
                       (\<exists>bound_c_21.
                         (((((((((distinct [bound_c_04 , bound_c_12 , bound_c_21]
                           \<and> (\<forall>bound_c6.
                             (((callOrigin bound_c6) = (Some bound_tx_04))
                               \<longrightarrow> (((bound_c6 = bound_c_04) \<or> (bound_c6 = bound_c_12)) \<or> (bound_c6 = bound_c_21)))))
                           \<and> ((callOrigin bound_c_04) = (Some bound_tx_04)))
                           \<and> ((callOrigin bound_c_12) = (Some bound_tx_04)))
                           \<and> ((callOrigin bound_c_21) = (Some bound_tx_04)))
                           \<and> ((calls bound_c_04)
                             = (Qry (messageQry (ContainsKey_MessageId_StructAuthorContentQuery bound_param_message_id)))))
                           \<and> ((calls bound_c_12) = (Op (chat (Remove_MessageId bound_param_message_id)))))
                           \<and> ((calls bound_c_21)
                             = (Op (message (DeleteKey_MessageId_StructAuthorContentOp bound_param_message_id)))))
                           \<and> (bound_c_04 \<in> (happensBefore bound_c_12)))
                           \<and> (bound_c_12 \<in> (happensBefore bound_c_21))))))))))))"

assumes before_procedure_invocation_invariant_shape_of_invocation_editMessage:

        "(\<forall>bound_invoc1.
         (\<forall>bound_param_id.
           (\<forall>bound_param_newContent.
             (((invocationOp bound_invoc1) = (editMessage bound_param_id bound_param_newContent))
               \<longrightarrow> ((((distinct [] \<and> (\<forall>bound_tx3. \<not>((transactionOrigin bound_tx3) = (Some bound_invoc1)))) \<and> distinct [])
                 \<or> (\<exists>bound_tx_01.
                   ((distinct [bound_tx_01]
                     \<and> (\<forall>bound_tx4. (((transactionOrigin bound_tx4) = (Some bound_invoc1)) \<longrightarrow> (bound_tx_01 = bound_tx4))))
                     \<and> (\<exists>bound_c_01.
                       (((distinct [bound_c_01]
                         \<and> (\<forall>bound_c3. (((callOrigin bound_c3) = (Some bound_tx_01)) \<longrightarrow> (bound_c3 = bound_c_01))))
                         \<and> ((callOrigin bound_c_01) = (Some bound_tx_01)))
                         \<and> ((calls bound_c_01)
                           = (Qry (messageQry (ContainsKey_MessageId_StructAuthorContentQuery bound_param_id)))))))))
                 \<or> (\<exists>bound_tx_02.
                   ((distinct [bound_tx_02]
                     \<and> (\<forall>bound_tx5. (((transactionOrigin bound_tx5) = (Some bound_invoc1)) \<longrightarrow> (bound_tx_02 = bound_tx5))))
                     \<and> (\<exists>bound_c_02.
                       (\<exists>bound_c_11.
                         ((((((distinct [bound_c_02 , bound_c_11]
                           \<and> (\<forall>bound_c4.
                             (((callOrigin bound_c4) = (Some bound_tx_02))
                               \<longrightarrow> ((bound_c4 = bound_c_02) \<or> (bound_c4 = bound_c_11)))))
                           \<and> ((callOrigin bound_c_02) = (Some bound_tx_02)))
                           \<and> ((callOrigin bound_c_11) = (Some bound_tx_02)))
                           \<and> ((calls bound_c_02)
                             = (Qry (messageQry (ContainsKey_MessageId_StructAuthorContentQuery bound_param_id)))))
                           \<and> ((calls bound_c_11)
                             = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_param_id
                                   (content (Assign_String bound_param_newContent)))))))
                           \<and> (bound_c_02 \<in> (happensBefore bound_c_11))))))))))))"

assumes before_procedure_invocation_invariant_shape_of_invocation_sendMessage:

        "(\<forall>bound_invoc.
         (\<forall>bound_param_from.
           (\<forall>bound_param_text.
             (((invocationOp bound_invoc) = (sendMessage bound_param_from bound_param_text))
               \<longrightarrow> (((distinct [] \<and> (\<forall>bound_tx1. \<not>((transactionOrigin bound_tx1) = (Some bound_invoc)))) \<and> distinct [])
                 \<or> (\<exists>bound_tx_0.
                   ((distinct [bound_tx_0]
                     \<and> (\<forall>bound_tx2. (((transactionOrigin bound_tx2) = (Some bound_invoc)) \<longrightarrow> (bound_tx_0 = bound_tx2))))
                     \<and> (\<exists>bound_c_0.
                       (\<exists>bound_c_1.
                         (\<exists>bound_c_2.
                           (((((((((distinct [bound_c_0 , bound_c_1 , bound_c_2]
                             \<and> (\<forall>bound_c.
                               (((callOrigin bound_c) = (Some bound_tx_0))
                                 \<longrightarrow> (((bound_c = bound_c_0) \<or> (bound_c = bound_c_1)) \<or> (bound_c = bound_c_2)))))
                             \<and> ((callOrigin bound_c_0) = (Some bound_tx_0)))
                             \<and> ((callOrigin bound_c_1) = (Some bound_tx_0)))
                             \<and> ((callOrigin bound_c_2) = (Some bound_tx_0)))
                             \<and> (\<exists>bound_arg_0_v_1.
                               ((calls bound_c_0)
                                 = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_arg_0_v_1
                                       (author (Assign_UserId bound_param_from))))))))
                             \<and> (\<exists>bound_arg_0_v_11.
                               ((calls bound_c_1)
                                 = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_arg_0_v_11
                                       (content (Assign_String bound_param_text))))))))
                             \<and> (\<exists>bound_arg_0_v_12. ((calls bound_c_2) = (Op (chat (Add_MessageId bound_arg_0_v_12))))))
                             \<and> (bound_c_0 \<in> (happensBefore bound_c_1)))
                             \<and> (bound_c_1 \<in> (happensBefore bound_c_2)))))))))))))"

assumes before_procedure_invocation_invariant_inv2:

        "\<not>(\<exists>bound_write.
         (\<exists>bound_delete.
           (\<exists>bound_m3.
             (\<exists>bound_upd.
               ((((calls bound_write) = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_m3 bound_upd))))
                 \<and> ((calls bound_delete) = (Op (message (DeleteKey_MessageId_StructAuthorContentOp bound_m3)))))
                 \<and> (bound_delete \<in> (happensBefore bound_write)))))))"

assumes before_procedure_invocation_invariant_inv1:

        "(\<forall>bound_c1.
         (\<forall>bound_m2.
           (\<forall>bound_s1.
             (((calls bound_c1)
               = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_m2 (content (Assign_String bound_s1))))))
               \<longrightarrow> (\<exists>bound_c2.
                 (\<exists>bound_u.
                   (((calls bound_c2)
                     = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_m2 (author (Assign_UserId bound_u))))))
                     \<and> (bound_c2 \<in> (happensBefore bound_c1)))))))))"

assumes before_procedure_invocation_invariant_inv0:

        "(\<forall>bound_g.
         (\<forall>bound_m1.
           (\<forall>bound_author1.
             (\<forall>bound_content1.
               ((((invocationOp bound_g) = (getMessage bound_m1))
                 \<and> ((invocationRes bound_g) = (getMessage_res (found bound_author1 bound_content1))))
                 \<longrightarrow> (\<exists>bound_s. (\<exists>bound_content2. ((invocationOp bound_s) = (sendMessage bound_author1 bound_content2)))))))))"

assumes i_fresh:

        "((invocationOp currentInvocation) = no_invocation)"

assumes unique_ids_rootCrdtQuery_messageQry_def:

        "(\<forall>bound_nested7.
         ((uniqueIds_op_MessageId_rootCrdtQuery (messageQry bound_nested7))
           = (uniqueIds_op_MessageId_MapQuery_MessageId_StructAuthorContentQuery bound_nested7)))"

assumes unique_ids_rootCrdtQuery_chatQry_def:

        "(\<forall>bound_nested4.
         ((uniqueIds_op_MessageId_rootCrdtQuery (chatQry bound_nested4))
           = (uniqueIds_op_MessageId_SetQuery_MessageId bound_nested4)))"

assumes unique_ids_StructAuthorContentQuery_contentQry_def:

        "(\<forall>bound_nested6.
         ((uniqueIds_op_MessageId_StructAuthorContentQuery (contentQry bound_nested6))
           = (uniqueIds_op_MessageId_MVRegisterQry_String bound_nested6)))"

assumes unique_ids_StructAuthorContentQuery_authorQry_def:

        "(\<forall>bound_nested5.
         ((uniqueIds_op_MessageId_StructAuthorContentQuery (authorQry bound_nested5))
           = (uniqueIds_op_MessageId_RegisterQry_UserId bound_nested5)))"

assumes unique_ids_RegisterOp_UserId_Assign_UserId_def:

        "(\<forall>bound_value.
         ((uniqueIds_op_MessageId_RegisterOp_UserId (Assign_UserId bound_value))
           = (uniqueIds_op_MessageId_SortCustomUninterpreted_UserId_1 bound_value)))"

assumes unique_ids_MVRegisterQry_String_MvContains_String_def:

        "(\<forall>bound_value2.
         ((uniqueIds_op_MessageId_MVRegisterQry_String (MvContains_String bound_value2))
           = (uniqueIds_op_MessageId_SortCustomUninterpreted_String_1 bound_value2)))"

assumes unique_ids_MVRegisterQry_String_ReadFirst_String_def:

        "((uniqueIds_op_MessageId_MVRegisterQry_String (ReadFirst_String )) = {})"

assumes uniqueIds_op_MessageId_SortCustomUninterpreted_UserId__def:

        "(\<forall>bound_x5. ((uniqueIds_op_MessageId_SortCustomUninterpreted_UserId_1 bound_x5) = {}))"

assumes uniqueIds_op_MessageId_SortCustomUninterpreted_MessageId__def:

        "(\<forall>bound_x4. ((uniqueIds_op_MessageId_SortCustomUninterpreted_MessageId_1 bound_x4) = {bound_x4}))"

assumes unique_ids_StructAuthorContentOp_content_def:

        "(\<forall>bound_nested2.
         ((uniqueIds_op_MessageId_StructAuthorContentOp (content bound_nested2))
           = (uniqueIds_op_MessageId_RegisterOp_String bound_nested2)))"

assumes unique_ids_StructAuthorContentOp_author_def:

        "(\<forall>bound_nested1.
         ((uniqueIds_op_MessageId_StructAuthorContentOp (author bound_nested1))
           = (uniqueIds_op_MessageId_RegisterOp_UserId bound_nested1)))"

assumes unique_ids_getMessageResult_found_def:

        "(\<forall>bound_author.
         (\<forall>bound_content.
           ((uniqueIds_op_MessageId_getMessageResult (found bound_author bound_content))
             = ((uniqueIds_op_MessageId_SortCustomUninterpreted_UserId_1 bound_author) \<union> (uniqueIds_op_MessageId_SortCustomUninterpreted_String_1 bound_content)))))"

assumes unique_ids_getMessageResult_notFound_def:

        "((uniqueIds_op_MessageId_getMessageResult (notFound )) = {})"

assumes unique_ids_rootCrdtOp_message_def:

        "(\<forall>bound_nested3.
         ((uniqueIds_op_MessageId_rootCrdtOp (message bound_nested3))
           = (uniqueIds_op_MessageId_MapOp_MessageId_StructAuthorContentOp bound_nested3)))"

assumes unique_ids_rootCrdtOp_chat_def:

        "(\<forall>bound_nested.
         ((uniqueIds_op_MessageId_rootCrdtOp (chat bound_nested)) = (uniqueIds_op_MessageId_SetOp_MessageId bound_nested)))"

assumes unique_ids_RegisterQry_UserId_ReadRegister_UserId_def:

        "((uniqueIds_op_MessageId_RegisterQry_UserId (ReadRegister_UserId )) = {})"

assumes uniqueIds_op_MessageId_SortCustomUninterpreted_String__def:

        "(\<forall>bound_x3. ((uniqueIds_op_MessageId_SortCustomUninterpreted_String_1 bound_x3) = {}))"

assumes unique_ids_MapOp_MessageId_StructAuthorContentOp_NestedOp_MessageId_StructAuthorContentOp_def:

        "(\<forall>bound_key1.
         (\<forall>bound_op.
           ((uniqueIds_op_MessageId_MapOp_MessageId_StructAuthorContentOp (NestedOp_MessageId_StructAuthorContentOp bound_key1
                 bound_op))
             = ((uniqueIds_op_MessageId_SortCustomUninterpreted_MessageId_1 bound_key1) \<union> (uniqueIds_op_MessageId_StructAuthorContentOp bound_op)))))"

assumes unique_ids_MapOp_MessageId_StructAuthorContentOp_DeleteKey_MessageId_StructAuthorContentOp_def:

        "(\<forall>bound_key.
         ((uniqueIds_op_MessageId_MapOp_MessageId_StructAuthorContentOp (DeleteKey_MessageId_StructAuthorContentOp bound_key))
           = (uniqueIds_op_MessageId_SortCustomUninterpreted_MessageId_1 bound_key)))"

assumes unique_ids_SetOp_MessageId_Remove_MessageId_def:

        "(\<forall>bound_x1.
         ((uniqueIds_op_MessageId_SetOp_MessageId (Remove_MessageId bound_x1))
           = (uniqueIds_op_MessageId_SortCustomUninterpreted_MessageId_1 bound_x1)))"

assumes unique_ids_SetOp_MessageId_Add_MessageId_def:

        "(\<forall>bound_x.
         ((uniqueIds_op_MessageId_SetOp_MessageId (Add_MessageId bound_x))
           = (uniqueIds_op_MessageId_SortCustomUninterpreted_MessageId_1 bound_x)))"

assumes unique_ids_RegisterOp_String_Assign_String_def:

        "(\<forall>bound_value1.
         ((uniqueIds_op_MessageId_RegisterOp_String (Assign_String bound_value1))
           = (uniqueIds_op_MessageId_SortCustomUninterpreted_String_1 bound_value1)))"

assumes unique_ids_MapQuery_MessageId_StructAuthorContentQuery_NestedQuery_MessageId_StructAuthorContentQuery_def:

        "(\<forall>bound_key3.
         (\<forall>bound_q.
           ((uniqueIds_op_MessageId_MapQuery_MessageId_StructAuthorContentQuery (NestedQuery_MessageId_StructAuthorContentQuery bound_key3
                 bound_q))
             = ((uniqueIds_op_MessageId_SortCustomUninterpreted_MessageId_1 bound_key3) \<union> (uniqueIds_op_MessageId_StructAuthorContentQuery bound_q)))))"

assumes unique_ids_MapQuery_MessageId_StructAuthorContentQuery_ContainsKey_MessageId_StructAuthorContentQuery_def:

        "(\<forall>bound_key2.
         ((uniqueIds_op_MessageId_MapQuery_MessageId_StructAuthorContentQuery (ContainsKey_MessageId_StructAuthorContentQuery bound_key2))
           = (uniqueIds_op_MessageId_SortCustomUninterpreted_MessageId_1 bound_key2)))"

assumes unique_ids_SetQuery_MessageId_Contains_MessageId_def:

        "(\<forall>bound_x2.
         ((uniqueIds_op_MessageId_SetQuery_MessageId (Contains_MessageId bound_x2))
           = (uniqueIds_op_MessageId_SortCustomUninterpreted_MessageId_1 bound_x2)))"

assumes unique_ids_InvocationResult_NoResult_def:

        "((uniqueIds_op_MessageId_SortInvocationRes_1 (NoResult )) = {})"

assumes unique_ids_InvocationResult_editMessage_res_def:

        "((uniqueIds_op_MessageId_SortInvocationRes_1 (editMessage_res )) = {})"

assumes unique_ids_InvocationResult_sendMessage_res_def:

        "(\<forall>bound_sendMessage_res_arg.
         ((uniqueIds_op_MessageId_SortInvocationRes_1 (sendMessage_res bound_sendMessage_res_arg))
           = (uniqueIds_op_MessageId_SortCustomUninterpreted_MessageId_1 bound_sendMessage_res_arg)))"

assumes unique_ids_InvocationResult_deleteMessage_res_def:

        "((uniqueIds_op_MessageId_SortInvocationRes_1 (deleteMessage_res )) = {})"

assumes unique_ids_InvocationResult_getMessage_res_def:

        "(\<forall>bound_getMessage_res_arg.
         ((uniqueIds_op_MessageId_SortInvocationRes_1 (getMessage_res bound_getMessage_res_arg))
           = (uniqueIds_op_MessageId_getMessageResult bound_getMessage_res_arg)))"

assumes unique_ids_InvocationInfo_no_invocation_def:

        "((uniqueIds_op_MessageId_SortInvocationInfo_1 (no_invocation )) = {})"

assumes unique_ids_InvocationInfo_deleteMessage_def:

        "(\<forall>bound_message_id.
         ((uniqueIds_op_MessageId_SortInvocationInfo_1 (deleteMessage bound_message_id))
           = (uniqueIds_op_MessageId_SortCustomUninterpreted_MessageId_1 bound_message_id)))"

assumes unique_ids_InvocationInfo_editMessage_def:

        "(\<forall>bound_id.
         (\<forall>bound_newContent.
           ((uniqueIds_op_MessageId_SortInvocationInfo_1 (editMessage bound_id bound_newContent))
             = ((uniqueIds_op_MessageId_SortCustomUninterpreted_MessageId_1 bound_id) \<union> (uniqueIds_op_MessageId_SortCustomUninterpreted_String_1 bound_newContent)))))"

assumes unique_ids_InvocationInfo_getMessage_def:

        "(\<forall>bound_m.
         ((uniqueIds_op_MessageId_SortInvocationInfo_1 (getMessage bound_m))
           = (uniqueIds_op_MessageId_SortCustomUninterpreted_MessageId_1 bound_m)))"

assumes unique_ids_InvocationInfo_sendMessage_def:

        "(\<forall>bound_from.
         (\<forall>bound_text.
           ((uniqueIds_op_MessageId_SortInvocationInfo_1 (sendMessage bound_from bound_text))
             = ((uniqueIds_op_MessageId_SortCustomUninterpreted_UserId_1 bound_from) \<union> (uniqueIds_op_MessageId_SortCustomUninterpreted_String_1 bound_text)))))"

assumes unique_ids_CallInfo_Qry_def:

        "(\<forall>bound_query.
         ((uniqueIds_op_MessageId_SortCall_1 (Qry bound_query)) = (uniqueIds_op_MessageId_rootCrdtQuery bound_query)))"

assumes unique_ids_CallInfo_NoCall_def:

        "((uniqueIds_op_MessageId_SortCall_1 (NoCall )) = {})"

assumes unique_ids_CallInfo_Op_def:

        "(\<forall>bound_operation.
         ((uniqueIds_op_MessageId_SortCall_1 (Op bound_operation)) = (uniqueIds_op_MessageId_rootCrdtOp bound_operation)))"

assumes old_transactions_unchanged:

        "(\<forall>c5.
         (\<forall>tx4.
           (((((calls c5) = NoCall) \<and> ((calls1 c5) \<noteq> NoCall)) \<and> ((callOrigin1 c5) = (Some tx4)))
             \<longrightarrow> ((transactionOrigin tx4) = None))))"

assumes growth_invocation_res:

        "(\<forall>i1. (((invocationRes i1) \<noteq> NoResult) \<longrightarrow> ((invocationRes1 i1) = (invocationRes i1))))"

assumes growth_invocation_op:

        "(\<forall>i.
         ((((invocationOp(currentInvocation := (getMessage m_init))) i) \<noteq> no_invocation)
           \<longrightarrow> ((invocationOp1 i) = ((invocationOp(currentInvocation := (getMessage m_init))) i))))"

assumes growth_tx_origin:

        "(\<forall>tx3. (\<not>((transactionOrigin tx3) = None) \<longrightarrow> ((transactionOrigin1 tx3) = (transactionOrigin tx3))))"

assumes growth_call_tx:

        "(\<forall>c4. (((calls c4) \<noteq> NoCall) \<longrightarrow> ((callOrigin1 c4) = (callOrigin c4))))"

assumes growth_happensbefore:

        "(\<forall>c3. (((calls c3) \<noteq> NoCall) \<longrightarrow> ((happensBefore1 c3) = (happensBefore c3))))"

assumes growth_calls:

        "(\<forall>c2. (((calls c2) \<noteq> NoCall) \<longrightarrow> ((calls1 c2) = (calls c2))))"

assumes growth_callOrigin:

        "(\<forall>c. (\<forall>tx2. (((callOrigin c) = (Some tx2)) \<longrightarrow> ((callOrigin1 c) = (Some tx2)))))"

assumes transaction_begin_snapshot_addition_transaction_consistent:

        "(\<forall>bound_c124.
         (\<forall>bound_c225.
           (((bound_c124 \<in> snapshotAddition1) \<and> (bound_c225 \<in> (happensBefore1 bound_c124))) \<longrightarrow> (bound_c225 \<in> snapshotAddition1))))"

assumes transaction_begin_snapshot_addition_transaction_consistent_2:

        "(\<forall>bound_c123.
         (\<forall>bound_c224.
           (((bound_c123 \<in> snapshotAddition1) \<and> ((callOrigin1 bound_c123) = (callOrigin1 bound_c224)))
             \<longrightarrow> (bound_c224 \<in> snapshotAddition1))))"

assumes transaction_begin_snapshot_addition_subset_calls:

        "(\<forall>bound_c77. ((bound_c77 \<in> snapshotAddition1) \<longrightarrow> ((calls1 bound_c77) \<noteq> NoCall)))"

assumes transaction_begin_MessageId_knownIds_are_generated:

        "(\<forall>bound_x17. ((bound_x17 \<in> knownIds_MessageId1) \<longrightarrow> \<not>((generatedIds_MessageId1 bound_x17) = None)))"

assumes transaction_begin_call_parameters_generated:

        "(\<forall>bound_c76.
         (\<forall>bound_uid1.
           ((bound_uid1 \<in> (uniqueIds_op_MessageId_SortCall_1 (calls1 bound_c76)))
             \<longrightarrow> ((generatedIds_MessageId1 bound_uid1) \<noteq> None))))"

assumes transaction_begin_database_calls_MessageId_are_generated:

        "(\<forall>bound_c75.
         (\<forall>bound_x16.
           ((bound_x16 \<in> (uniqueIds_op_MessageId_SortCall_1 (calls1 bound_c75))) \<longrightarrow> ((generatedIds_MessageId1 bound_x16) \<noteq> None))))"

assumes transaction_begin_getMessage_MessageId_res_known:

        "(\<forall>bound_i23. ((uniqueIds_op_MessageId_SortInvocationRes_1 (invocationRes1 bound_i23)) \<subseteq> knownIds_MessageId1))"

assumes transaction_begin_getMessage_MessageId_args_known:

        "(\<forall>bound_i22. ((uniqueIds_op_MessageId_SortInvocationInfo_1 (invocationOp1 bound_i22)) \<subseteq> knownIds_MessageId1))"

assumes transaction_begin_deleteMessage_MessageId_res_known:

        "(\<forall>bound_i21. ((uniqueIds_op_MessageId_SortInvocationRes_1 (invocationRes1 bound_i21)) \<subseteq> knownIds_MessageId1))"

assumes transaction_begin_deleteMessage_MessageId_args_known:

        "(\<forall>bound_i20. ((uniqueIds_op_MessageId_SortInvocationInfo_1 (invocationOp1 bound_i20)) \<subseteq> knownIds_MessageId1))"

assumes transaction_begin_editMessage_MessageId_res_known:

        "(\<forall>bound_i19. ((uniqueIds_op_MessageId_SortInvocationRes_1 (invocationRes1 bound_i19)) \<subseteq> knownIds_MessageId1))"

assumes transaction_begin_editMessage_MessageId_args_known:

        "(\<forall>bound_i18. ((uniqueIds_op_MessageId_SortInvocationInfo_1 (invocationOp1 bound_i18)) \<subseteq> knownIds_MessageId1))"

assumes transaction_begin_sendMessage_MessageId_res_known:

        "(\<forall>bound_i17. ((uniqueIds_op_MessageId_SortInvocationRes_1 (invocationRes1 bound_i17)) \<subseteq> knownIds_MessageId1))"

assumes transaction_begin_sendMessage_MessageId_args_known:

        "(\<forall>bound_i16. ((uniqueIds_op_MessageId_SortInvocationInfo_1 (invocationOp1 bound_i16)) \<subseteq> knownIds_MessageId1))"

assumes transaction_begin_WF_transactionOrigin_exists:

        "(\<forall>bound_tx96.
         (\<forall>bound_i15. (((transactionOrigin1 bound_tx96) = (Some bound_i15)) \<longrightarrow> ((invocationOp1 bound_i15) \<noteq> no_invocation))))"

assumes transaction_begin_WF_callOrigin_exists:

        "(\<forall>bound_ca1.
         (\<forall>bound_tx95. (((callOrigin1 bound_ca1) = (Some bound_tx95)) \<longrightarrow> \<not>((transactionOrigin1 bound_tx95) = None))))"

assumes transaction_begin_WF_no_call_implies_no_happensBefore:

        "(\<forall>bound_c74. (((calls1 bound_c74) = NoCall) \<longrightarrow> ((happensBefore1 bound_c74) = {})))"

assumes transaction_begin_WF_transactionOrigin_callOrigin:

        "(\<forall>bound_tx94. (((transactionOrigin1 bound_tx94) = None) \<longrightarrow> (\<forall>bound_c73. ((callOrigin1 bound_c73) \<noteq> (Some bound_tx94)))))"

assumes transaction_begin_WF_callOrigin:

        "(\<forall>bound_c72. (((callOrigin1 bound_c72) = None) = ((calls1 bound_c72) = NoCall)))"

assumes transaction_begin_WF_invocationCalls:

        "(\<forall>bound_i14.
         (\<forall>bound_c71.
           ((bound_c71 \<in> (invocationCalls1 bound_i14))
             = (\<exists>bound_tx93.
             (((callOrigin1 bound_c71) = (Some bound_tx93)) \<and> ((transactionOrigin1 bound_tx93) = (Some bound_i14)))))))"

assumes transaction_begin_happens_before_transaction_consistent_r:

        "(\<forall>bound_x15.
         (\<forall>bound_y14.
           (\<forall>bound_y24.
             (((((callOrigin1 bound_y14) = (callOrigin1 bound_y24)) \<and> \<not>((callOrigin1 bound_x15) = (callOrigin1 bound_y14)))
               \<and> (bound_x15 \<in> (happensBefore1 bound_y14)))
               \<longrightarrow> (bound_x15 \<in> (happensBefore1 bound_y24))))))"

assumes transaction_begin_happens_before_transaction_consistent_l:

        "(\<forall>bound_x14.
         (\<forall>bound_y13.
           (\<forall>bound_y23.
             (((((callOrigin1 bound_y13) = (callOrigin1 bound_y23)) \<and> \<not>((callOrigin1 bound_x14) = (callOrigin1 bound_y13)))
               \<and> (bound_y13 \<in> (happensBefore1 bound_x14)))
               \<longrightarrow> (bound_y23 \<in> (happensBefore1 bound_x14))))))"

assumes transaction_begin_no_invocation_implies_no_result:

        "(\<forall>bound_i13. (((invocationOp1 bound_i13) = no_invocation) \<longrightarrow> ((invocationRes1 bound_i13) = NoResult)))"

assumes transaction_begin_happensBefore_antisym:

        "(\<forall>bound_x13. (\<forall>bound_y4. ((bound_x13 \<in> (happensBefore1 bound_y4)) \<longrightarrow> \<not>(bound_y4 \<in> (happensBefore1 bound_x13)))))"

assumes transaction_begin_happensBefore_trans:

        "(\<forall>bound_x12.
         (\<forall>bound_y3.
           (\<forall>bound_z1.
             (((bound_x12 \<in> (happensBefore1 bound_y3)) \<and> (bound_y3 \<in> (happensBefore1 bound_z1)))
               \<longrightarrow> (bound_x12 \<in> (happensBefore1 bound_z1))))))"

assumes transaction_begin_happensBefore_non_reflex:

        "(\<forall>bound_c70. \<not>(bound_c70 \<in> (happensBefore1 bound_c70)))"

assumes transaction_begin_visibleCalls_causally_consistent:

        "(\<forall>bound_c122. (\<forall>bound_c223. (((bound_c223 \<in> vis) \<and> (bound_c122 \<in> (happensBefore1 bound_c223))) \<longrightarrow> (bound_c122 \<in> vis))))"

assumes transaction_begin_visibleCalls_transaction_consistent1:

        "(\<forall>bound_c121.
         (\<forall>bound_c222.
           ((((bound_c121 \<in> vis) \<and> ((callOrigin1 bound_c121) = (callOrigin1 bound_c222))) \<and> ((calls1 bound_c222) \<noteq> NoCall))
             \<longrightarrow> (bound_c222 \<in> vis))))"

assumes transaction_begin_visibleCalls_exist:

        "(\<forall>bound_c69. ((bound_c69 \<in> vis) \<longrightarrow> ((calls1 bound_c69) \<noteq> NoCall)))"

assumes transaction_begin_invocation_sequential:

        "(\<forall>bound_c120.
         (\<forall>bound_tx111.
           (\<forall>bound_i12.
             (\<forall>bound_c221.
               (\<forall>bound_tx211.
                 (((((((callOrigin1 bound_c120) = (Some bound_tx111)) \<and> ((transactionOrigin1 bound_tx111) = (Some bound_i12)))
                   \<and> ((callOrigin1 bound_c221) = (Some bound_tx211)))
                   \<and> ((transactionOrigin1 bound_tx211) = (Some bound_i12)))
                   \<and> (bound_c120 \<noteq> bound_c221))
                   \<longrightarrow> ((bound_c120 \<in> (happensBefore1 bound_c221)) \<or> (bound_c221 \<in> (happensBefore1 bound_c120)))))))))"

assumes transaction_begin_happensBefore_exists_r:

        "(\<forall>bound_c119. (\<forall>bound_c220. ((bound_c119 \<in> (happensBefore1 bound_c220)) \<longrightarrow> ((calls1 bound_c220) \<noteq> NoCall))))"

assumes transaction_begin_happensBefore_exists_l:

        "(\<forall>bound_c118. (\<forall>bound_c219. ((bound_c118 \<in> (happensBefore1 bound_c219)) \<longrightarrow> ((calls1 bound_c118) \<noteq> NoCall))))"

assumes at_transaction_begin_invariant_shape_rev_ShapeCall_Qry_messageQry_NestedQuery_MessageId_StructAuthorContentQuery_x_19_contentQry_MvContains_String_x_22_q:

        "(\<forall>bound_c68.
         (\<forall>bound_arg_0_x_1911.
           (\<forall>bound_arg_1_x_221.
             \<not>((calls1 bound_c68)
               = (Qry (messageQry (NestedQuery_MessageId_StructAuthorContentQuery bound_arg_0_x_1911
                     (contentQry (MvContains_String bound_arg_1_x_221)))))))))"

assumes at_transaction_begin_invariant_shape_rev_ShapeCall_Qry_messageQry_NestedQuery_MessageId_StructAuthorContentQuery_x_19_contentQry_ReadFirst_String_q:

        "(\<forall>bound_c67.
         (\<forall>bound_arg_0_x_1910.
           (((calls1 bound_c67)
             = (Qry (messageQry (NestedQuery_MessageId_StructAuthorContentQuery bound_arg_0_x_1910
                   (contentQry (ReadFirst_String ))))))
             \<longrightarrow> (\<exists>bound_invoc73.
               (((case (callOrigin1 bound_c67) of    Some bound_tx92 => (transactionOrigin1 bound_tx92) | None => None)
                 = (Some bound_invoc73))
                 \<and> ((invocationOp1 bound_invoc73) = (getMessage bound_arg_0_x_1910)))))))"

assumes at_transaction_begin_invariant_shape_rev_ShapeCall_Qry_messageQry_NestedQuery_MessageId_StructAuthorContentQuery_x_19_contentQry_x_21_q:

        "(\<forall>bound_c66.
         (\<forall>bound_arg_0_x_199.
           (\<forall>bound_arg_1_x_211.
             (((calls1 bound_c66)
               = (Qry (messageQry (NestedQuery_MessageId_StructAuthorContentQuery bound_arg_0_x_199
                     (contentQry bound_arg_1_x_211)))))
               \<longrightarrow> (\<exists>bound_invoc72.
                 (((case (callOrigin1 bound_c66) of    Some bound_tx91 => (transactionOrigin1 bound_tx91) | None => None)
                   = (Some bound_invoc72))
                   \<and> ((invocationOp1 bound_invoc72) = (getMessage bound_arg_0_x_199))))))))"

assumes at_transaction_begin_invariant_shape_rev_ShapeCall_Qry_messageQry_NestedQuery_MessageId_StructAuthorContentQuery_x_19_authorQry_ReadRegister_UserId_q:

        "(\<forall>bound_c65.
         (\<forall>bound_arg_0_x_198.
           (((calls1 bound_c65)
             = (Qry (messageQry (NestedQuery_MessageId_StructAuthorContentQuery bound_arg_0_x_198
                   (authorQry (ReadRegister_UserId ))))))
             \<longrightarrow> (\<exists>bound_invoc71.
               (((case (callOrigin1 bound_c65) of    Some bound_tx90 => (transactionOrigin1 bound_tx90) | None => None)
                 = (Some bound_invoc71))
                 \<and> ((invocationOp1 bound_invoc71) = (getMessage bound_arg_0_x_198)))))))"

assumes at_transaction_begin_invariant_shape_rev_ShapeCall_Qry_messageQry_NestedQuery_MessageId_StructAuthorContentQuery_x_19_authorQry_x_20_q:

        "(\<forall>bound_c64.
         (\<forall>bound_arg_0_x_197.
           (\<forall>bound_arg_1_x_201.
             (((calls1 bound_c64)
               = (Qry (messageQry (NestedQuery_MessageId_StructAuthorContentQuery bound_arg_0_x_197
                     (authorQry bound_arg_1_x_201)))))
               \<longrightarrow> (\<exists>bound_invoc70.
                 (((case (callOrigin1 bound_c64) of    Some bound_tx89 => (transactionOrigin1 bound_tx89) | None => None)
                   = (Some bound_invoc70))
                   \<and> ((invocationOp1 bound_invoc70) = (getMessage bound_arg_0_x_197))))))))"

assumes at_transaction_begin_invariant_shape_rev_ShapeCall_Qry_messageQry_NestedQuery_MessageId_StructAuthorContentQuery_x_19_x_18_q:

        "(\<forall>bound_c63.
         (\<forall>bound_arg_0_x_196.
           (\<forall>bound_arg_1_x_181.
             (((calls1 bound_c63)
               = (Qry (messageQry (NestedQuery_MessageId_StructAuthorContentQuery bound_arg_0_x_196 bound_arg_1_x_181))))
               \<longrightarrow> (\<exists>bound_invoc69.
                 (((case (callOrigin1 bound_c63) of    Some bound_tx88 => (transactionOrigin1 bound_tx88) | None => None)
                   = (Some bound_invoc69))
                   \<and> ((invocationOp1 bound_invoc69) = (getMessage bound_arg_0_x_196))))))))"

assumes at_transaction_begin_invariant_shape_rev_ShapeCall_Qry_messageQry_ContainsKey_MessageId_StructAuthorContentQuery_x_17_q:

        "(\<forall>bound_c62.
         (\<forall>bound_arg_0_x_171.
           (((calls1 bound_c62) = (Qry (messageQry (ContainsKey_MessageId_StructAuthorContentQuery bound_arg_0_x_171))))
             \<longrightarrow> (((\<exists>bound_invoc66.
               (\<exists>bound_newContent14.
                 (((case (callOrigin1 bound_c62) of    Some bound_tx85 => (transactionOrigin1 bound_tx85) | None => None)
                   = (Some bound_invoc66))
                   \<and> ((invocationOp1 bound_invoc66) = (editMessage bound_arg_0_x_171 bound_newContent14)))))
               \<or> (\<exists>bound_invoc67.
                 (((case (callOrigin1 bound_c62) of    Some bound_tx86 => (transactionOrigin1 bound_tx86) | None => None)
                   = (Some bound_invoc67))
                   \<and> ((invocationOp1 bound_invoc67) = (deleteMessage bound_arg_0_x_171)))))
               \<or> (\<exists>bound_invoc68.
                 (((case (callOrigin1 bound_c62) of    Some bound_tx87 => (transactionOrigin1 bound_tx87) | None => None)
                   = (Some bound_invoc68))
                   \<and> ((invocationOp1 bound_invoc68) = (getMessage bound_arg_0_x_171))))))))"

assumes at_transaction_begin_invariant_shape_rev_ShapeCall_Qry_messageQry_x_16_q:

        "(\<forall>bound_c61.
         (\<forall>bound_arg_0_x_161.
           (((calls1 bound_c61) = (Qry (messageQry bound_arg_0_x_161)))
             \<longrightarrow> (((\<exists>bound_invoc63.
               (\<exists>bound_id8.
                 (\<exists>bound_newContent13.
                   (((case (callOrigin1 bound_c61) of    Some bound_tx82 => (transactionOrigin1 bound_tx82) | None => None)
                     = (Some bound_invoc63))
                     \<and> ((invocationOp1 bound_invoc63) = (editMessage bound_id8 bound_newContent13))))))
               \<or> (\<exists>bound_invoc64.
                 (\<exists>bound_message_id10.
                   (((case (callOrigin1 bound_c61) of    Some bound_tx83 => (transactionOrigin1 bound_tx83) | None => None)
                     = (Some bound_invoc64))
                     \<and> ((invocationOp1 bound_invoc64) = (deleteMessage bound_message_id10))))))
               \<or> (\<exists>bound_invoc65.
                 (\<exists>bound_m11.
                   (((case (callOrigin1 bound_c61) of    Some bound_tx84 => (transactionOrigin1 bound_tx84) | None => None)
                     = (Some bound_invoc65))
                     \<and> ((invocationOp1 bound_invoc65) = (getMessage bound_m11)))))))))"

assumes at_transaction_begin_invariant_shape_rev_ShapeCall_Qry_chatQry_Contains_MessageId_x_15_q:

        "(\<forall>bound_c60. (\<forall>bound_arg_0_x_151. \<not>((calls1 bound_c60) = (Qry (chatQry (Contains_MessageId bound_arg_0_x_151))))))"

assumes at_transaction_begin_invariant_shape_rev_ShapeCall_Qry_chatQry_x_14_q:

        "(\<forall>bound_c59. (\<forall>bound_arg_0_x_141. \<not>((calls1 bound_c59) = (Qry (chatQry bound_arg_0_x_141)))))"

assumes at_transaction_begin_invariant_shape_rev_ShapeCall_Qry_x_13_q:

        "(\<forall>bound_c58.
         (\<forall>bound_arg_0_x_131.
           (((calls1 bound_c58) = (Qry bound_arg_0_x_131))
             \<longrightarrow> (((\<exists>bound_invoc60.
               (\<exists>bound_id7.
                 (\<exists>bound_newContent12.
                   (((case (callOrigin1 bound_c58) of    Some bound_tx79 => (transactionOrigin1 bound_tx79) | None => None)
                     = (Some bound_invoc60))
                     \<and> ((invocationOp1 bound_invoc60) = (editMessage bound_id7 bound_newContent12))))))
               \<or> (\<exists>bound_invoc61.
                 (\<exists>bound_message_id9.
                   (((case (callOrigin1 bound_c58) of    Some bound_tx80 => (transactionOrigin1 bound_tx80) | None => None)
                     = (Some bound_invoc61))
                     \<and> ((invocationOp1 bound_invoc61) = (deleteMessage bound_message_id9))))))
               \<or> (\<exists>bound_invoc62.
                 (\<exists>bound_m10.
                   (((case (callOrigin1 bound_c58) of    Some bound_tx81 => (transactionOrigin1 bound_tx81) | None => None)
                     = (Some bound_invoc62))
                     \<and> ((invocationOp1 bound_invoc62) = (getMessage bound_m10)))))))))"

assumes at_transaction_begin_invariant_shape_rev_ShapeCall_Op_message_NestedOp_MessageId_StructAuthorContentOp_x_8_content_Assign_String_x_12_q:

        "(\<forall>bound_c57.
         (\<forall>bound_arg_0_x_89.
           (\<forall>bound_arg_1_x_121.
             (((calls1 bound_c57)
               = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_arg_0_x_89
                     (content (Assign_String bound_arg_1_x_121))))))
               \<longrightarrow> ((\<exists>bound_invoc58.
                 (\<exists>bound_from16.
                   (((case (callOrigin1 bound_c57) of    Some bound_tx77 => (transactionOrigin1 bound_tx77) | None => None)
                     = (Some bound_invoc58))
                     \<and> ((invocationOp1 bound_invoc58) = (sendMessage bound_from16 bound_arg_1_x_121)))))
                 \<or> (\<exists>bound_invoc59.
                   (((case (callOrigin1 bound_c57) of    Some bound_tx78 => (transactionOrigin1 bound_tx78) | None => None)
                     = (Some bound_invoc59))
                     \<and> ((invocationOp1 bound_invoc59) = (editMessage bound_arg_0_x_89 bound_arg_1_x_121)))))))))"

assumes at_transaction_begin_invariant_shape_rev_ShapeCall_Op_message_NestedOp_MessageId_StructAuthorContentOp_x_8_content_x_11_q:

        "(\<forall>bound_c56.
         (\<forall>bound_arg_0_x_88.
           (\<forall>bound_arg_1_x_111.
             (((calls1 bound_c56)
               = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_arg_0_x_88 (content bound_arg_1_x_111)))))
               \<longrightarrow> ((\<exists>bound_invoc56.
                 (\<exists>bound_from15.
                   (\<exists>bound_text16.
                     (((case (callOrigin1 bound_c56) of    Some bound_tx75 => (transactionOrigin1 bound_tx75) | None => None)
                       = (Some bound_invoc56))
                       \<and> ((invocationOp1 bound_invoc56) = (sendMessage bound_from15 bound_text16))))))
                 \<or> (\<exists>bound_invoc57.
                   (\<exists>bound_newContent11.
                     (((case (callOrigin1 bound_c56) of    Some bound_tx76 => (transactionOrigin1 bound_tx76) | None => None)
                       = (Some bound_invoc57))
                       \<and> ((invocationOp1 bound_invoc57) = (editMessage bound_arg_0_x_88 bound_newContent11))))))))))"

assumes at_transaction_begin_invariant_shape_rev_ShapeCall_Op_message_NestedOp_MessageId_StructAuthorContentOp_x_8_author_Assign_UserId_x_10_q:

        "(\<forall>bound_c55.
         (\<forall>bound_arg_0_x_87.
           (\<forall>bound_arg_1_x_101.
             (((calls1 bound_c55)
               = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_arg_0_x_87
                     (author (Assign_UserId bound_arg_1_x_101))))))
               \<longrightarrow> (\<exists>bound_invoc55.
                 (\<exists>bound_text15.
                   (((case (callOrigin1 bound_c55) of    Some bound_tx74 => (transactionOrigin1 bound_tx74) | None => None)
                     = (Some bound_invoc55))
                     \<and> ((invocationOp1 bound_invoc55) = (sendMessage bound_arg_1_x_101 bound_text15)))))))))"

assumes at_transaction_begin_invariant_shape_rev_ShapeCall_Op_message_NestedOp_MessageId_StructAuthorContentOp_x_8_author_x_9_q:

        "(\<forall>bound_c54.
         (\<forall>bound_arg_0_x_86.
           (\<forall>bound_arg_1_x_91.
             (((calls1 bound_c54)
               = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_arg_0_x_86 (author bound_arg_1_x_91)))))
               \<longrightarrow> (\<exists>bound_invoc54.
                 (\<exists>bound_from14.
                   (\<exists>bound_text14.
                     (((case (callOrigin1 bound_c54) of    Some bound_tx73 => (transactionOrigin1 bound_tx73) | None => None)
                       = (Some bound_invoc54))
                       \<and> ((invocationOp1 bound_invoc54) = (sendMessage bound_from14 bound_text14))))))))))"

assumes at_transaction_begin_invariant_shape_rev_ShapeCall_Op_message_NestedOp_MessageId_StructAuthorContentOp_x_8_x_7_q:

        "(\<forall>bound_c53.
         (\<forall>bound_arg_0_x_85.
           (\<forall>bound_arg_1_x_71.
             (((calls1 bound_c53) = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_arg_0_x_85 bound_arg_1_x_71))))
               \<longrightarrow> ((\<exists>bound_invoc52.
                 (\<exists>bound_from13.
                   (\<exists>bound_text13.
                     (((case (callOrigin1 bound_c53) of    Some bound_tx71 => (transactionOrigin1 bound_tx71) | None => None)
                       = (Some bound_invoc52))
                       \<and> ((invocationOp1 bound_invoc52) = (sendMessage bound_from13 bound_text13))))))
                 \<or> (\<exists>bound_invoc53.
                   (\<exists>bound_newContent10.
                     (((case (callOrigin1 bound_c53) of    Some bound_tx72 => (transactionOrigin1 bound_tx72) | None => None)
                       = (Some bound_invoc53))
                       \<and> ((invocationOp1 bound_invoc53) = (editMessage bound_arg_0_x_85 bound_newContent10))))))))))"

assumes at_transaction_begin_invariant_shape_rev_ShapeCall_Op_message_DeleteKey_MessageId_StructAuthorContentOp_x_6_q:

        "(\<forall>bound_c52.
         (\<forall>bound_arg_0_x_61.
           (((calls1 bound_c52) = (Op (message (DeleteKey_MessageId_StructAuthorContentOp bound_arg_0_x_61))))
             \<longrightarrow> (\<exists>bound_invoc51.
               (((case (callOrigin1 bound_c52) of    Some bound_tx70 => (transactionOrigin1 bound_tx70) | None => None)
                 = (Some bound_invoc51))
                 \<and> ((invocationOp1 bound_invoc51) = (deleteMessage bound_arg_0_x_61)))))))"

assumes at_transaction_begin_invariant_shape_rev_ShapeCall_Op_message_x_5_q:

        "(\<forall>bound_c51.
         (\<forall>bound_arg_0_x_51.
           (((calls1 bound_c51) = (Op (message bound_arg_0_x_51)))
             \<longrightarrow> (((\<exists>bound_invoc48.
               (\<exists>bound_from12.
                 (\<exists>bound_text12.
                   (((case (callOrigin1 bound_c51) of    Some bound_tx67 => (transactionOrigin1 bound_tx67) | None => None)
                     = (Some bound_invoc48))
                     \<and> ((invocationOp1 bound_invoc48) = (sendMessage bound_from12 bound_text12))))))
               \<or> (\<exists>bound_invoc49.
                 (\<exists>bound_id6.
                   (\<exists>bound_newContent9.
                     (((case (callOrigin1 bound_c51) of    Some bound_tx68 => (transactionOrigin1 bound_tx68) | None => None)
                       = (Some bound_invoc49))
                       \<and> ((invocationOp1 bound_invoc49) = (editMessage bound_id6 bound_newContent9)))))))
               \<or> (\<exists>bound_invoc50.
                 (\<exists>bound_message_id8.
                   (((case (callOrigin1 bound_c51) of    Some bound_tx69 => (transactionOrigin1 bound_tx69) | None => None)
                     = (Some bound_invoc50))
                     \<and> ((invocationOp1 bound_invoc50) = (deleteMessage bound_message_id8)))))))))"

assumes at_transaction_begin_invariant_shape_rev_ShapeCall_Op_chat_Remove_MessageId_x_4_q:

        "(\<forall>bound_c50.
         (\<forall>bound_arg_0_x_41.
           (((calls1 bound_c50) = (Op (chat (Remove_MessageId bound_arg_0_x_41))))
             \<longrightarrow> (\<exists>bound_invoc47.
               (((case (callOrigin1 bound_c50) of    Some bound_tx66 => (transactionOrigin1 bound_tx66) | None => None)
                 = (Some bound_invoc47))
                 \<and> ((invocationOp1 bound_invoc47) = (deleteMessage bound_arg_0_x_41)))))))"

assumes at_transaction_begin_invariant_shape_rev_ShapeCall_Op_chat_Add_MessageId_x_3_q:

        "(\<forall>bound_c49.
         (\<forall>bound_arg_0_x_31.
           (((calls1 bound_c49) = (Op (chat (Add_MessageId bound_arg_0_x_31))))
             \<longrightarrow> (\<exists>bound_invoc46.
               (\<exists>bound_from11.
                 (\<exists>bound_text11.
                   (((case (callOrigin1 bound_c49) of    Some bound_tx65 => (transactionOrigin1 bound_tx65) | None => None)
                     = (Some bound_invoc46))
                     \<and> ((invocationOp1 bound_invoc46) = (sendMessage bound_from11 bound_text11)))))))))"

assumes at_transaction_begin_invariant_shape_rev_ShapeCall_Op_chat_x_2_q:

        "(\<forall>bound_c48.
         (\<forall>bound_arg_0_x_21.
           (((calls1 bound_c48) = (Op (chat bound_arg_0_x_21)))
             \<longrightarrow> ((\<exists>bound_invoc44.
               (\<exists>bound_from10.
                 (\<exists>bound_text10.
                   (((case (callOrigin1 bound_c48) of    Some bound_tx63 => (transactionOrigin1 bound_tx63) | None => None)
                     = (Some bound_invoc44))
                     \<and> ((invocationOp1 bound_invoc44) = (sendMessage bound_from10 bound_text10))))))
               \<or> (\<exists>bound_invoc45.
                 (\<exists>bound_message_id7.
                   (((case (callOrigin1 bound_c48) of    Some bound_tx64 => (transactionOrigin1 bound_tx64) | None => None)
                     = (Some bound_invoc45))
                     \<and> ((invocationOp1 bound_invoc45) = (deleteMessage bound_message_id7)))))))))"

assumes at_transaction_begin_invariant_shape_rev_ShapeCall_Op_x_1_q:

        "(\<forall>bound_c47.
         (\<forall>bound_arg_0_x_11.
           (((calls1 bound_c47) = (Op bound_arg_0_x_11))
             \<longrightarrow> (((\<exists>bound_invoc41.
               (\<exists>bound_from9.
                 (\<exists>bound_text9.
                   (((case (callOrigin1 bound_c47) of    Some bound_tx60 => (transactionOrigin1 bound_tx60) | None => None)
                     = (Some bound_invoc41))
                     \<and> ((invocationOp1 bound_invoc41) = (sendMessage bound_from9 bound_text9))))))
               \<or> (\<exists>bound_invoc42.
                 (\<exists>bound_id5.
                   (\<exists>bound_newContent8.
                     (((case (callOrigin1 bound_c47) of    Some bound_tx61 => (transactionOrigin1 bound_tx61) | None => None)
                       = (Some bound_invoc42))
                       \<and> ((invocationOp1 bound_invoc42) = (editMessage bound_id5 bound_newContent8)))))))
               \<or> (\<exists>bound_invoc43.
                 (\<exists>bound_message_id6.
                   (((case (callOrigin1 bound_c47) of    Some bound_tx62 => (transactionOrigin1 bound_tx62) | None => None)
                     = (Some bound_invoc43))
                     \<and> ((invocationOp1 bound_invoc43) = (deleteMessage bound_message_id6)))))))))"

assumes at_transaction_begin_invariant_shape_of_invocation_getMessage:

        "(\<forall>bound_invoc40.
         (\<forall>bound_param_m1.
           (((invocationOp1 bound_invoc40) = (getMessage bound_param_m1))
             \<longrightarrow> ((((distinct [] \<and> (\<forall>bound_tx57. \<not>((transactionOrigin1 bound_tx57) = (Some bound_invoc40)))) \<and> distinct [])
               \<or> (\<exists>bound_tx_012.
                 ((distinct [bound_tx_012]
                   \<and> (\<forall>bound_tx58. (((transactionOrigin1 bound_tx58) = (Some bound_invoc40)) \<longrightarrow> (bound_tx_012 = bound_tx58))))
                   \<and> (\<exists>bound_c_012.
                     (((distinct [bound_c_012]
                       \<and> (\<forall>bound_c45. (((callOrigin1 bound_c45) = (Some bound_tx_012)) \<longrightarrow> (bound_c45 = bound_c_012))))
                       \<and> ((callOrigin1 bound_c_012) = (Some bound_tx_012)))
                       \<and> ((calls1 bound_c_012)
                         = (Qry (messageQry (ContainsKey_MessageId_StructAuthorContentQuery bound_param_m1)))))))))
               \<or> (\<exists>bound_tx_013.
                 ((distinct [bound_tx_013]
                   \<and> (\<forall>bound_tx59. (((transactionOrigin1 bound_tx59) = (Some bound_invoc40)) \<longrightarrow> (bound_tx_013 = bound_tx59))))
                   \<and> (\<exists>bound_c_013.
                     (\<exists>bound_c_17.
                       (\<exists>bound_c_25.
                         (((((((((distinct [bound_c_013 , bound_c_17 , bound_c_25]
                           \<and> (\<forall>bound_c46.
                             (((callOrigin1 bound_c46) = (Some bound_tx_013))
                               \<longrightarrow> (((bound_c46 = bound_c_013) \<or> (bound_c46 = bound_c_17)) \<or> (bound_c46 = bound_c_25)))))
                           \<and> ((callOrigin1 bound_c_013) = (Some bound_tx_013)))
                           \<and> ((callOrigin1 bound_c_17) = (Some bound_tx_013)))
                           \<and> ((callOrigin1 bound_c_25) = (Some bound_tx_013)))
                           \<and> ((calls1 bound_c_013)
                             = (Qry (messageQry (ContainsKey_MessageId_StructAuthorContentQuery bound_param_m1)))))
                           \<and> ((calls1 bound_c_17)
                             = (Qry (messageQry (NestedQuery_MessageId_StructAuthorContentQuery bound_param_m1
                                   (authorQry (ReadRegister_UserId )))))))
                           \<and> ((calls1 bound_c_25)
                             = (Qry (messageQry (NestedQuery_MessageId_StructAuthorContentQuery bound_param_m1
                                   (contentQry (ReadFirst_String )))))))
                           \<and> (bound_c_013 \<in> (happensBefore1 bound_c_17)))
                           \<and> (bound_c_17 \<in> (happensBefore1 bound_c_25))))))))))))"

assumes at_transaction_begin_invariant_shape_of_invocation_deleteMessage:

        "(\<forall>bound_invoc39.
         (\<forall>bound_param_message_id1.
           (((invocationOp1 bound_invoc39) = (deleteMessage bound_param_message_id1))
             \<longrightarrow> ((((distinct [] \<and> (\<forall>bound_tx54. \<not>((transactionOrigin1 bound_tx54) = (Some bound_invoc39)))) \<and> distinct [])
               \<or> (\<exists>bound_tx_010.
                 ((distinct [bound_tx_010]
                   \<and> (\<forall>bound_tx55. (((transactionOrigin1 bound_tx55) = (Some bound_invoc39)) \<longrightarrow> (bound_tx_010 = bound_tx55))))
                   \<and> (\<exists>bound_c_010.
                     (((distinct [bound_c_010]
                       \<and> (\<forall>bound_c43. (((callOrigin1 bound_c43) = (Some bound_tx_010)) \<longrightarrow> (bound_c43 = bound_c_010))))
                       \<and> ((callOrigin1 bound_c_010) = (Some bound_tx_010)))
                       \<and> ((calls1 bound_c_010)
                         = (Qry (messageQry (ContainsKey_MessageId_StructAuthorContentQuery bound_param_message_id1)))))))))
               \<or> (\<exists>bound_tx_011.
                 ((distinct [bound_tx_011]
                   \<and> (\<forall>bound_tx56. (((transactionOrigin1 bound_tx56) = (Some bound_invoc39)) \<longrightarrow> (bound_tx_011 = bound_tx56))))
                   \<and> (\<exists>bound_c_011.
                     (\<exists>bound_c_16.
                       (\<exists>bound_c_24.
                         (((((((((distinct [bound_c_011 , bound_c_16 , bound_c_24]
                           \<and> (\<forall>bound_c44.
                             (((callOrigin1 bound_c44) = (Some bound_tx_011))
                               \<longrightarrow> (((bound_c44 = bound_c_011) \<or> (bound_c44 = bound_c_16)) \<or> (bound_c44 = bound_c_24)))))
                           \<and> ((callOrigin1 bound_c_011) = (Some bound_tx_011)))
                           \<and> ((callOrigin1 bound_c_16) = (Some bound_tx_011)))
                           \<and> ((callOrigin1 bound_c_24) = (Some bound_tx_011)))
                           \<and> ((calls1 bound_c_011)
                             = (Qry (messageQry (ContainsKey_MessageId_StructAuthorContentQuery bound_param_message_id1)))))
                           \<and> ((calls1 bound_c_16) = (Op (chat (Remove_MessageId bound_param_message_id1)))))
                           \<and> ((calls1 bound_c_24)
                             = (Op (message (DeleteKey_MessageId_StructAuthorContentOp bound_param_message_id1)))))
                           \<and> (bound_c_011 \<in> (happensBefore1 bound_c_16)))
                           \<and> (bound_c_16 \<in> (happensBefore1 bound_c_24))))))))))))"

assumes at_transaction_begin_invariant_shape_of_invocation_editMessage:

        "(\<forall>bound_invoc38.
         (\<forall>bound_param_id1.
           (\<forall>bound_param_newContent1.
             (((invocationOp1 bound_invoc38) = (editMessage bound_param_id1 bound_param_newContent1))
               \<longrightarrow> ((((distinct [] \<and> (\<forall>bound_tx51. \<not>((transactionOrigin1 bound_tx51) = (Some bound_invoc38)))) \<and> distinct [])
                 \<or> (\<exists>bound_tx_08.
                   ((distinct [bound_tx_08]
                     \<and> (\<forall>bound_tx52. (((transactionOrigin1 bound_tx52) = (Some bound_invoc38)) \<longrightarrow> (bound_tx_08 = bound_tx52))))
                     \<and> (\<exists>bound_c_08.
                       (((distinct [bound_c_08]
                         \<and> (\<forall>bound_c41. (((callOrigin1 bound_c41) = (Some bound_tx_08)) \<longrightarrow> (bound_c41 = bound_c_08))))
                         \<and> ((callOrigin1 bound_c_08) = (Some bound_tx_08)))
                         \<and> ((calls1 bound_c_08)
                           = (Qry (messageQry (ContainsKey_MessageId_StructAuthorContentQuery bound_param_id1)))))))))
                 \<or> (\<exists>bound_tx_09.
                   ((distinct [bound_tx_09]
                     \<and> (\<forall>bound_tx53. (((transactionOrigin1 bound_tx53) = (Some bound_invoc38)) \<longrightarrow> (bound_tx_09 = bound_tx53))))
                     \<and> (\<exists>bound_c_09.
                       (\<exists>bound_c_15.
                         ((((((distinct [bound_c_09 , bound_c_15]
                           \<and> (\<forall>bound_c42.
                             (((callOrigin1 bound_c42) = (Some bound_tx_09))
                               \<longrightarrow> ((bound_c42 = bound_c_09) \<or> (bound_c42 = bound_c_15)))))
                           \<and> ((callOrigin1 bound_c_09) = (Some bound_tx_09)))
                           \<and> ((callOrigin1 bound_c_15) = (Some bound_tx_09)))
                           \<and> ((calls1 bound_c_09)
                             = (Qry (messageQry (ContainsKey_MessageId_StructAuthorContentQuery bound_param_id1)))))
                           \<and> ((calls1 bound_c_15)
                             = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_param_id1
                                   (content (Assign_String bound_param_newContent1)))))))
                           \<and> (bound_c_09 \<in> (happensBefore1 bound_c_15))))))))))))"

assumes at_transaction_begin_invariant_shape_of_invocation_sendMessage:

        "(\<forall>bound_invoc37.
         (\<forall>bound_param_from1.
           (\<forall>bound_param_text1.
             (((invocationOp1 bound_invoc37) = (sendMessage bound_param_from1 bound_param_text1))
               \<longrightarrow> (((distinct [] \<and> (\<forall>bound_tx49. \<not>((transactionOrigin1 bound_tx49) = (Some bound_invoc37)))) \<and> distinct [])
                 \<or> (\<exists>bound_tx_07.
                   ((distinct [bound_tx_07]
                     \<and> (\<forall>bound_tx50. (((transactionOrigin1 bound_tx50) = (Some bound_invoc37)) \<longrightarrow> (bound_tx_07 = bound_tx50))))
                     \<and> (\<exists>bound_c_07.
                       (\<exists>bound_c_14.
                         (\<exists>bound_c_23.
                           (((((((((distinct [bound_c_07 , bound_c_14 , bound_c_23]
                             \<and> (\<forall>bound_c40.
                               (((callOrigin1 bound_c40) = (Some bound_tx_07))
                                 \<longrightarrow> (((bound_c40 = bound_c_07) \<or> (bound_c40 = bound_c_14)) \<or> (bound_c40 = bound_c_23)))))
                             \<and> ((callOrigin1 bound_c_07) = (Some bound_tx_07)))
                             \<and> ((callOrigin1 bound_c_14) = (Some bound_tx_07)))
                             \<and> ((callOrigin1 bound_c_23) = (Some bound_tx_07)))
                             \<and> (\<exists>bound_arg_0_v_13.
                               ((calls1 bound_c_07)
                                 = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_arg_0_v_13
                                       (author (Assign_UserId bound_param_from1))))))))
                             \<and> (\<exists>bound_arg_0_v_14.
                               ((calls1 bound_c_14)
                                 = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_arg_0_v_14
                                       (content (Assign_String bound_param_text1))))))))
                             \<and> (\<exists>bound_arg_0_v_15. ((calls1 bound_c_23) = (Op (chat (Add_MessageId bound_arg_0_v_15))))))
                             \<and> (bound_c_07 \<in> (happensBefore1 bound_c_14)))
                             \<and> (bound_c_14 \<in> (happensBefore1 bound_c_23)))))))))))))"

assumes at_transaction_begin_invariant_inv2:

        "\<not>(\<exists>bound_write2.
         (\<exists>bound_delete2.
           (\<exists>bound_m9.
             (\<exists>bound_upd2.
               ((((calls1 bound_write2) = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_m9 bound_upd2))))
                 \<and> ((calls1 bound_delete2) = (Op (message (DeleteKey_MessageId_StructAuthorContentOp bound_m9)))))
                 \<and> (bound_delete2 \<in> (happensBefore1 bound_write2)))))))"

assumes at_transaction_begin_invariant_inv1:

        "(\<forall>bound_c117.
         (\<forall>bound_m8.
           (\<forall>bound_s4.
             (((calls1 bound_c117)
               = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_m8 (content (Assign_String bound_s4))))))
               \<longrightarrow> (\<exists>bound_c218.
                 (\<exists>bound_u2.
                   (((calls1 bound_c218)
                     = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_m8 (author (Assign_UserId bound_u2))))))
                     \<and> (bound_c218 \<in> (happensBefore1 bound_c117)))))))))"

assumes at_transaction_begin_invariant_inv0:

        "(\<forall>bound_g1.
         (\<forall>bound_m7.
           (\<forall>bound_author2.
             (\<forall>bound_content3.
               ((((invocationOp1 bound_g1) = (getMessage bound_m7))
                 \<and> ((invocationRes1 bound_g1) = (getMessage_res (found bound_author2 bound_content3))))
                 \<longrightarrow> (\<exists>bound_s3. (\<exists>bound_content22. ((invocationOp1 bound_s3) = (sendMessage bound_author2 bound_content22)))))))))"

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

assumes messageQry_ContainsKey_res_def:

        "(messageQry_ContainsKey_res
         = ((\<exists>bound_e.
         ((bound_e \<in> vis)
           \<and> (\<exists>bound_n. ((calls1 bound_e) = (Op (message (NestedOp_MessageId_StructAuthorContentOp m_init bound_n)))))))
         \<and> (\<forall>bound_d.
           (((bound_d \<in> vis) \<and> ((calls1 bound_d) = (Op (message (DeleteKey_MessageId_StructAuthorContentOp m_init)))))
             \<longrightarrow> (\<exists>bound_e1.
               (((bound_e1 \<in> vis)
                 \<and> (\<exists>bound_n1. ((calls1 bound_e1) = (Op (message (NestedOp_MessageId_StructAuthorContentOp m_init bound_n1))))))
                 \<and> (bound_d \<in> (happensBefore1 bound_e1))))))))"

assumes q__query_messageQry_ContainsKey_res_1_assignment:

        "(q__query_messageQry_ContainsKey_res_1 = messageQry_ContainsKey_res)"

assumes c0_freshB:

        "((calls1 c0) = NoCall)"

assumes c0_freshA:

        "distinct [c0]"

assumes if_statement_condition_true:

        "q__query_messageQry_ContainsKey_res_1"

assumes q__query_messageQry_NestedQuery_authorQry_ReadRegister_res_2_assignment:

        "(q__query_messageQry_NestedQuery_authorQry_ReadRegister_res_2 = messageQry_NestedQuery_authorQry_ReadRegister)"

assumes vis_def:

        "(vis1 = (vis \<union> {c0}))"

assumes calls_def:

        "(calls2 = (calls1(c0 := (Qry (messageQry (ContainsKey_MessageId_StructAuthorContentQuery m_init))))))"

assumes happensBefore_def:

        "(happensBefore2 = (happensBefore1(c0 := vis)))"

assumes query_messageQry_NestedQuery_authorQry_ReadRegister_postcondition_def:

        "(query_messageQry_NestedQuery_authorQry_ReadRegister_postcondition
         = (\<not>(\<exists>bound_c78.
         (\<exists>bound_v.
           (((bound_c78 \<in> vis1)
             \<and> (\<forall>bound_r.
               ((((calls2 bound_r) = (Op (message (DeleteKey_MessageId_StructAuthorContentOp m_init)))) \<and> (bound_r \<in> vis1))
                 \<longrightarrow> (bound_r \<in> (happensBefore2 bound_c78)))))
             \<and> ((calls2 bound_c78)
               = (Op (message (NestedOp_MessageId_StructAuthorContentOp m_init (author (Assign_UserId bound_v)))))))))
         \<or> (\<exists>bound_c79.
           ((((bound_c79 \<in> vis1)
             \<and> (\<forall>bound_r1.
               ((((calls2 bound_r1) = (Op (message (DeleteKey_MessageId_StructAuthorContentOp m_init)))) \<and> (bound_r1 \<in> vis1))
                 \<longrightarrow> (bound_r1 \<in> (happensBefore2 bound_c79)))))
             \<and> ((calls2 bound_c79)
               = (Op (message (NestedOp_MessageId_StructAuthorContentOp m_init
                     (author (Assign_UserId messageQry_NestedQuery_authorQry_ReadRegister)))))))
             \<and> \<not>(\<exists>bound_c226.
               (\<exists>bound_v1.
                 ((((bound_c226 \<in> vis1)
                   \<and> (\<forall>bound_r2.
                     ((((calls2 bound_r2) = (Op (message (DeleteKey_MessageId_StructAuthorContentOp m_init))))
                       \<and> (bound_r2 \<in> vis1))
                       \<longrightarrow> (bound_r2 \<in> (happensBefore2 bound_c226)))))
                   \<and> (bound_c79 \<in> (happensBefore2 bound_c226)))
                   \<and> ((calls2 bound_c226)
                     = (Op (message (NestedOp_MessageId_StructAuthorContentOp m_init (author (Assign_UserId bound_v1)))))))))))))"

assumes choose_messageQry_NestedQuery_authorQry_ReadRegister:

        "query_messageQry_NestedQuery_authorQry_ReadRegister_postcondition"

assumes c11_freshB:

        "((calls2 c11) = NoCall)"

assumes c11_freshA:

        "distinct [c11 , c0]"

assumes q__query_messageQry_NestedQuery_contentQry_ReadFirst_res_3_assignment:

        "(q__query_messageQry_NestedQuery_contentQry_ReadFirst_res_3 = messageQry_NestedQuery_contentQry_ReadFirst)"

assumes vis_def_2:

        "(vis2 = (vis1 \<union> {c11}))"

assumes calls_def_2:

        "(calls3
         = (calls2(c11 := (Qry (messageQry (NestedQuery_MessageId_StructAuthorContentQuery m_init
               (authorQry (ReadRegister_UserId ))))))))"

assumes happensBefore_def_2:

        "(happensBefore3 = (happensBefore2(c11 := vis1)))"

assumes query_messageQry_NestedQuery_contentQry_ReadFirst_postcondition_def:

        "(query_messageQry_NestedQuery_contentQry_ReadFirst_postcondition
         = (\<not>(\<exists>bound_c80.
         (\<exists>bound_v2.
           (((bound_c80 \<in> vis2)
             \<and> (\<forall>bound_r3.
               ((((calls3 bound_r3) = (Op (message (DeleteKey_MessageId_StructAuthorContentOp m_init)))) \<and> (bound_r3 \<in> vis2))
                 \<longrightarrow> (bound_r3 \<in> (happensBefore3 bound_c80)))))
             \<and> ((calls3 bound_c80)
               = (Op (message (NestedOp_MessageId_StructAuthorContentOp m_init (content (Assign_String bound_v2)))))))))
         \<or> (\<exists>bound_c81.
           ((((bound_c81 \<in> vis2)
             \<and> (\<forall>bound_r4.
               ((((calls3 bound_r4) = (Op (message (DeleteKey_MessageId_StructAuthorContentOp m_init)))) \<and> (bound_r4 \<in> vis2))
                 \<longrightarrow> (bound_r4 \<in> (happensBefore3 bound_c81)))))
             \<and> ((calls3 bound_c81)
               = (Op (message (NestedOp_MessageId_StructAuthorContentOp m_init
                     (content (Assign_String messageQry_NestedQuery_contentQry_ReadFirst)))))))
             \<and> \<not>(\<exists>bound_c227.
               (\<exists>bound_v3.
                 ((((bound_c227 \<in> vis2)
                   \<and> (\<forall>bound_r5.
                     ((((calls3 bound_r5) = (Op (message (DeleteKey_MessageId_StructAuthorContentOp m_init))))
                       \<and> (bound_r5 \<in> vis2))
                       \<longrightarrow> (bound_r5 \<in> (happensBefore3 bound_c227)))))
                   \<and> (bound_c81 \<in> (happensBefore3 bound_c227)))
                   \<and> ((calls3 bound_c227)
                     = (Op (message (NestedOp_MessageId_StructAuthorContentOp m_init (content (Assign_String bound_v3)))))))))))))"

assumes choose_messageQry_NestedQuery_contentQry_ReadFirst:

        "query_messageQry_NestedQuery_contentQry_ReadFirst_postcondition"

assumes c21_freshB:

        "((calls3 c21) = NoCall)"

assumes c21_freshA:

        "distinct [c21 , c0 , c11]"
assumes invariant_not_violated:

        "\<not>((((invocationOp1 g1) = (getMessage m3))
         \<and> (((invocationRes1(currentInvocation := (getMessage_res (found q__query_messageQry_NestedQuery_authorQry_ReadRegister_res_2
             q__query_messageQry_NestedQuery_contentQry_ReadFirst_res_3)))) g1)
           = (getMessage_res (found author2 content2))))
         \<longrightarrow> (\<exists>bound_s5. (\<exists>bound_content23. ((invocationOp1 bound_s5) = (sendMessage author2 bound_content23)))))"
         shows False
  using invariant_not_violated
proof (auto split: if_splits)
  assume a0: "g1 \<noteq> currentInvocation"
    and a1: "invocationOp1 g1 = getMessage m3"
    and a2: "invocationRes1 g1 = getMessage_res (found author2 content2)"
    and a3: "\<forall>bound_s5 bound_content23. invocationOp1 bound_s5 \<noteq> sendMessage author2 bound_content23"

  text \<open>Holds by previous invariant\<close>
  show "False"
    using a2 at_transaction_begin_invariant_inv0 a1 a3 by blast
next

  assume a0: "g1 = currentInvocation"
    and a1: "invocationOp1 currentInvocation = getMessage m3"
    and a2: "q__query_messageQry_NestedQuery_authorQry_ReadRegister_res_2 = author2"
    and a3: "q__query_messageQry_NestedQuery_contentQry_ReadFirst_res_3 = content2"
    and a4: "\<forall>bound_s5 bound_content23. invocationOp1 bound_s5 \<noteq> sendMessage author2 bound_content23"


  text \<open>Exists query returns true, so there must be an update:\<close>
  have "messageQry_ContainsKey_res"
    using if_statement_condition_true q__query_messageQry_ContainsKey_res_1_assignment by auto

  text \<open>There must be a non-deleted call updating author.\<close>

  with messageQry_ContainsKey_res_def
  obtain upd updOp
    where "upd \<in> vis"
      and "calls1 upd = Op (message (NestedOp_MessageId_StructAuthorContentOp m_init updOp))"
    by auto

  from messageQry_ContainsKey_res_def \<open>messageQry_ContainsKey_res\<close>
  have no_delete: "calls1 d \<noteq> Op (message (DeleteKey_MessageId_StructAuthorContentOp m_init))" if "d\<in>vis" for d
    using at_transaction_begin_invariant_inv2 that by blast

  hence  no_delete2: "calls2 d \<noteq> Op (message (DeleteKey_MessageId_StructAuthorContentOp m_init))" if "d\<in>vis1" for d
    using c0_freshB that vis_def calls_def by auto

  have \<open>upd \<in> vis1\<close>
    by (simp add: vis_def \<open>upd \<in> vis\<close>)

  text \<open>From the invariant we get that there is an update to the author:\<close>
  from at_transaction_begin_invariant_inv1
  obtain updA updA_author
    where "updA \<in> vis"
      and "calls1 updA = Op (message (NestedOp_MessageId_StructAuthorContentOp m_init (author (Assign_UserId updA_author))))"
    by (metis RegisterOp_String.exhaust RegisterOp_UserId.collapse StructAuthorContentOp.exhaust \<open>calls1 upd = Op (message (NestedOp_MessageId_StructAuthorContentOp m_init updOp))\<close> \<open>upd \<in> vis\<close>  transaction_begin_visibleCalls_causally_consistent)



  text \<open>Use query result of author to show that there is a call with the returned author\<close>

  from query_messageQry_NestedQuery_authorQry_ReadRegister_postcondition_def
    \<open>query_messageQry_NestedQuery_authorQry_ReadRegister_postcondition\<close>
  obtain updA'
    where "updA' \<in> vis1"
      and "calls2 updA' = Op (message (NestedOp_MessageId_StructAuthorContentOp m_init (author (Assign_UserId author2))))"
    apply auto
    subgoal
      by (metis Set.set_insert Un_insert_left \<open>\<And>thesis. (\<And>updA updA_author. \<lbrakk>updA \<in> vis; calls1 updA = Op (message (NestedOp_MessageId_StructAuthorContentOp m_init (author (Assign_UserId updA_author))))\<rbrakk> \<Longrightarrow> thesis) \<Longrightarrow> thesis\<close> c0_freshB calls_def fun_upd_same fun_upd_triv fun_upd_twist insertI1 no_delete2 transaction_begin_visibleCalls_exist vis_def)
    subgoal
      using a2 q__query_messageQry_NestedQuery_authorQry_ReadRegister_res_2_assignment by blast
    done

  hence authorUpdate:
    "calls1 updA' = Op (message (NestedOp_MessageId_StructAuthorContentOp m_init (author (Assign_UserId author2))))"
    by (metis callInfo.distinct(3) calls_def fun_upd_other fun_upd_same)



  text \<open>Use shape invariant to get to invoc: \<close>

  from at_transaction_begin_invariant_shape_rev_ShapeCall_Op_message_NestedOp_MessageId_StructAuthorContentOp_x_8_author_Assign_UserId_x_10_q[rule_format, OF authorUpdate]
  have "\<exists>invoc content. invocationOp1 invoc = sendMessage author2 content"
    by auto
 
    
  thus "False"
    using a4 by auto

qed


