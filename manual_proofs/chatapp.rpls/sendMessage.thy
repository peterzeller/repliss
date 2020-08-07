
theory "sendMessage"
  imports Main
begin

      datatype CallId = CallId nat

datatype TxId = TxId nat

datatype MessageId = MessageId nat

datatype SetOp_MessageId =
    Add_MessageId (x1: "MessageId")
  | Remove_MessageId (x2: "MessageId")

datatype UserId = UserId nat

datatype RegisterOp_UserId =
    Assign_UserId (value2: "UserId")

datatype String = String nat

datatype RegisterOp_String =
    Assign_String (value1: "String")

datatype StructAuthorContentOp =
    author (nested2: "RegisterOp_UserId")
  | content (nested3: "RegisterOp_String")

datatype MapOp_MessageId_StructAuthorContentOp =
    DeleteKey_MessageId_StructAuthorContentOp (key2: "MessageId")
  | NestedOp_MessageId_StructAuthorContentOp (key3: "MessageId") (op: "StructAuthorContentOp")

datatype rootCrdtOp =
    chat (nested4: "SetOp_MessageId")
  | message (nested5: "MapOp_MessageId_StructAuthorContentOp")

datatype SetQuery_MessageId =
    Contains_MessageId (x: "MessageId")

datatype RegisterQry_UserId =
    ReadRegister_UserId 

datatype MVRegisterQry_String =
    ReadFirst_String 
  | MvContains_String (qvalue: "String")

datatype StructAuthorContentQuery =
    authorQry (nested: "RegisterQry_UserId")
  | contentQry (nested1: "MVRegisterQry_String")

datatype MapQuery_MessageId_StructAuthorContentQuery =
    ContainsKey_MessageId_StructAuthorContentQuery (key: "MessageId")
  | NestedQuery_MessageId_StructAuthorContentQuery (key1: "MessageId") (q: "StructAuthorContentQuery")

datatype rootCrdtQuery =
    chatQry (nested6: "SetQuery_MessageId")
  | messageQry (nested7: "MapQuery_MessageId_StructAuthorContentQuery")

datatype callInfo =
    Op (operation: "rootCrdtOp")
  | NoCall 
  | Qry (query: "rootCrdtQuery")

datatype InvocationId = InvocationId nat

datatype invocationResult =
    sendMessage_res (sendMessage_res_arg: "MessageId")
  | NoResult 

datatype invocationInfo =
    sendMessage (qfrom: "UserId") (qtext: "String")
  | no_invocation 

lemma "sendMessage_line3":
fixes happensBefore3 :: "CallId => CallId set"
         
fixes vis1 :: "CallId set"
         
fixes calls1 :: "CallId => callInfo"
         
fixes happensBefore4 :: "CallId => CallId set"
         
fixes newCalls :: "CallId set"
         
fixes snapshotAddition :: "CallId set"
         
fixes happensBefore1 :: "CallId => CallId set"
         
fixes generatedIds_MessageId1 :: "MessageId => InvocationId option"
         
fixes from_init :: "UserId"
         
fixes calls4 :: "CallId => callInfo"
         
fixes knownIds_MessageId1 :: "MessageId set"
         
fixes transactionOrigin1 :: "TxId => InvocationId option"
         
fixes c11 :: "CallId"
         
fixes tx1 :: "TxId"
         
fixes invocationRes :: "InvocationId => invocationResult"
         
fixes invocationOp :: "InvocationId => invocationInfo"
         
fixes callOrigin :: "CallId => TxId option"
         
fixes invocationRes1 :: "InvocationId => invocationResult"
         
fixes invocationCalls :: "InvocationId => CallId set"
         
fixes calls :: "CallId => callInfo"
         
fixes calls3 :: "CallId => callInfo"
         
fixes m :: "MessageId"
         
fixes vis :: "CallId set"
         
fixes vis2 :: "CallId set"
         
fixes transactionOrigin :: "TxId => InvocationId option"
         
fixes callOrigin1 :: "CallId => TxId option"
         
fixes calls2 :: "CallId => callInfo"
         
fixes c21 :: "CallId"
         
fixes text_init :: "String"
         
fixes generatedIds_MessageId :: "MessageId => InvocationId option"
         
fixes invocationOp1 :: "InvocationId => invocationInfo"
         
fixes snapshotAddition1 :: "CallId set"
         
fixes currentInvocation :: "InvocationId"
         
fixes invocationCalls1 :: "InvocationId => CallId set"
         
fixes c0 :: "CallId"
         
fixes knownIds_MessageId :: "MessageId set"
         
fixes happensBefore2 :: "CallId => CallId set"
         
fixes happensBefore :: "CallId => CallId set"
         
fixes newTxns :: "TxId set"
         
assumes before_procedure_invocation_snapshot_addition_transaction_consistent:

        "(\<forall>bound_c16.
         (\<forall>bound_c26.
           (((bound_c16 \<in> snapshotAddition) \<and> (bound_c26 \<in> (happensBefore bound_c16))) \<longrightarrow> (bound_c26 \<in> snapshotAddition))))"
         
assumes before_procedure_invocation_snapshot_addition_transaction_consistent_2:

        "(\<forall>bound_c15.
         (\<forall>bound_c25.
           (((bound_c15 \<in> snapshotAddition) \<and> ((callOrigin bound_c15) = (callOrigin bound_c25)))
             \<longrightarrow> (bound_c25 \<in> snapshotAddition))))"
         
assumes before_procedure_invocation_snapshot_addition_subset_calls:

        "(\<forall>bound_c10. ((bound_c10 \<in> snapshotAddition) \<longrightarrow> ((calls bound_c10) \<noteq> NoCall)))"
         
assumes before_procedure_invocation_MessageId_knownIds_are_generated:

        "(\<forall>bound_x23. ((bound_x23 \<in> knownIds_MessageId) \<longrightarrow> \<not>((generatedIds_MessageId bound_x23) = None)))"
         
assumes before_procedure_invocation_call_parameters_generated:

        "(\<forall>bound_c9.
         (\<forall>bound_uid.
           ((bound_uid \<in> (uniqueIds_op_MessageId_SortCall_1 (calls bound_c9))) \<longrightarrow> ((generatedIds_MessageId bound_uid) \<noteq> None))))"
         
assumes before_procedure_invocation_database_calls_MessageId_are_generated:

        "(\<forall>bound_c8.
         (\<forall>bound_x22.
           ((bound_x22 \<in> (uniqueIds_op_MessageId_SortCall_1 (calls bound_c8))) \<longrightarrow> ((generatedIds_MessageId bound_x22) \<noteq> None))))"
         
assumes before_procedure_invocation_sendMessage_MessageId_res_known:

        "(\<forall>bound_i5. ((uniqueIds_op_MessageId_SortInvocationRes_1 (invocationRes bound_i5)) \<subseteq> knownIds_MessageId))"
         
assumes before_procedure_invocation_sendMessage_MessageId_args_known:

        "(\<forall>bound_i4. ((uniqueIds_op_MessageId_SortInvocationInfo_1 (invocationOp bound_i4)) \<subseteq> knownIds_MessageId))"
         
assumes before_procedure_invocation_WF_transactionOrigin_exists:

        "(\<forall>bound_tx6.
         (\<forall>bound_i3. (((transactionOrigin bound_tx6) = (Some bound_i3)) \<longrightarrow> ((invocationOp bound_i3) \<noteq> no_invocation))))"
         
assumes before_procedure_invocation_WF_callOrigin_exists:

        "(\<forall>bound_ca. (\<forall>bound_tx5. (((callOrigin bound_ca) = (Some bound_tx5)) \<longrightarrow> \<not>((transactionOrigin bound_tx5) = None))))"
         
assumes before_procedure_invocation_WF_no_call_implies_no_happensBefore:

        "(\<forall>bound_c7. (((calls bound_c7) = NoCall) \<longrightarrow> ((happensBefore bound_c7) = {})))"
         
assumes before_procedure_invocation_WF_transactionOrigin_callOrigin:

        "(\<forall>bound_tx4. (((transactionOrigin bound_tx4) = None) \<longrightarrow> (\<forall>bound_c6. ((callOrigin bound_c6) \<noteq> (Some bound_tx4)))))"
         
assumes before_procedure_invocation_WF_callOrigin:

        "(\<forall>bound_c5. (((callOrigin bound_c5) = None) = ((calls bound_c5) = NoCall)))"
         
assumes before_procedure_invocation_WF_invocationCalls:

        "(\<forall>bound_i2.
         (\<forall>bound_c4.
           ((bound_c4 \<in> (invocationCalls bound_i2))
             = (\<exists>bound_tx3. (((callOrigin bound_c4) = (Some bound_tx3)) \<and> ((transactionOrigin bound_tx3) = (Some bound_i2)))))))"
         
assumes before_procedure_invocation_happens_before_transaction_consistent_r:

        "(\<forall>bound_x21.
         (\<forall>bound_y12.
           (\<forall>bound_y22.
             (((((callOrigin bound_y12) = (callOrigin bound_y22)) \<and> \<not>((callOrigin bound_x21) = (callOrigin bound_y12)))
               \<and> (bound_x21 \<in> (happensBefore bound_y12)))
               \<longrightarrow> (bound_x21 \<in> (happensBefore bound_y22))))))"
         
assumes before_procedure_invocation_happens_before_transaction_consistent_l:

        "(\<forall>bound_x20.
         (\<forall>bound_y11.
           (\<forall>bound_y21.
             (((((callOrigin bound_y11) = (callOrigin bound_y21)) \<and> \<not>((callOrigin bound_x20) = (callOrigin bound_y11)))
               \<and> (bound_y11 \<in> (happensBefore bound_x20)))
               \<longrightarrow> (bound_y21 \<in> (happensBefore bound_x20))))))"
         
assumes before_procedure_invocation_no_invocation_implies_no_result:

        "(\<forall>bound_i1. (((invocationOp bound_i1) = no_invocation) \<longrightarrow> ((invocationRes bound_i1) = NoResult)))"
         
assumes before_procedure_invocation_happensBefore_antisym:

        "(\<forall>bound_x19. (\<forall>bound_y1. ((bound_x19 \<in> (happensBefore bound_y1)) \<longrightarrow> \<not>(bound_y1 \<in> (happensBefore bound_x19)))))"
         
assumes before_procedure_invocation_happensBefore_trans:

        "(\<forall>bound_x18.
         (\<forall>bound_y.
           (\<forall>bound_z.
             (((bound_x18 \<in> (happensBefore bound_y)) \<and> (bound_y \<in> (happensBefore bound_z)))
               \<longrightarrow> (bound_x18 \<in> (happensBefore bound_z))))))"
         
assumes before_procedure_invocation_happensBefore_non_reflex:

        "(\<forall>bound_c3. \<not>(bound_c3 \<in> (happensBefore bound_c3)))"
         
assumes before_procedure_invocation_visibleCalls_causally_consistent:

        "(\<forall>bound_c14. (\<forall>bound_c24. (((bound_c24 \<in> {}) \<and> (bound_c14 \<in> (happensBefore bound_c24))) \<longrightarrow> (bound_c14 \<in> {}))))"
         
assumes before_procedure_invocation_visibleCalls_transaction_consistent1:

        "(\<forall>bound_c13.
         (\<forall>bound_c23.
           ((((bound_c13 \<in> {}) \<and> ((callOrigin bound_c13) = (callOrigin bound_c23))) \<and> ((calls bound_c23) \<noteq> NoCall))
             \<longrightarrow> (bound_c23 \<in> {}))))"
         
assumes before_procedure_invocation_visibleCalls_exist:

        "(\<forall>bound_c. ((bound_c \<in> {}) \<longrightarrow> ((calls bound_c) \<noteq> NoCall)))"
         
assumes before_procedure_invocation_invocation_sequential:

        "(\<forall>bound_c12.
         (\<forall>bound_tx1.
           (\<forall>bound_i.
             (\<forall>bound_c22.
               (\<forall>bound_tx2.
                 (((((((callOrigin bound_c12) = (Some bound_tx1)) \<and> ((transactionOrigin bound_tx1) = (Some bound_i)))
                   \<and> ((callOrigin bound_c22) = (Some bound_tx2)))
                   \<and> ((transactionOrigin bound_tx2) = (Some bound_i)))
                   \<and> (bound_c12 \<noteq> bound_c22))
                   \<longrightarrow> ((bound_c12 \<in> (happensBefore bound_c22)) \<or> (bound_c22 \<in> (happensBefore bound_c12)))))))))"
         
assumes before_procedure_invocation_happensBefore_exists_r:

        "(\<forall>bound_c11. (\<forall>bound_c21. ((bound_c11 \<in> (happensBefore bound_c21)) \<longrightarrow> ((calls bound_c21) \<noteq> NoCall))))"
         
assumes before_procedure_invocation_happensBefore_exists_l:

        "(\<forall>bound_c1. (\<forall>bound_c2. ((bound_c1 \<in> (happensBefore bound_c2)) \<longrightarrow> ((calls bound_c1) \<noteq> NoCall))))"
         
assumes no_call_in_new_invocation:

        "((invocationCalls currentInvocation) = {})"
         
assumes no_transaction_in_new_invocation:

        "(\<forall>tx. ((transactionOrigin tx) \<noteq> (Some currentInvocation)))"
         
assumes before_procedure_invocation_invariant_inv0:

        "\<not>(\<exists>bound_write.
         (\<exists>bound_delete.
           (\<exists>bound_m.
             (\<exists>bound_upd.
               ((((calls bound_write) = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_m bound_upd))))
                 \<and> ((calls bound_delete) = (Op (message (DeleteKey_MessageId_StructAuthorContentOp bound_m)))))
                 \<and> (bound_delete \<in> (happensBefore bound_write)))))))"
         
assumes i_fresh:

        "((invocationOp currentInvocation) = no_invocation)"
         
assumes uniqueIds_op_MessageId_rootCrdtQuery_def:

        "((\<forall>bound_nested6.
         ((uniqueIds_op_MessageId_rootCrdtQuery (chatQry bound_nested6))
           = (uniqueIds_op_MessageId_SetQuery_MessageId bound_nested6)))
         \<and> (\<forall>bound_nested7.
           ((uniqueIds_op_MessageId_rootCrdtQuery (messageQry bound_nested7))
             = (uniqueIds_op_MessageId_MapQuery_MessageId_StructAuthorContentQuery bound_nested7))))"
         
assumes uniqueIds_op_MessageId_StructAuthorContentQuery_def:

        "((\<forall>bound_nested4.
         ((uniqueIds_op_MessageId_StructAuthorContentQuery (authorQry bound_nested4))
           = (uniqueIds_op_MessageId_RegisterQry_UserId bound_nested4)))
         \<and> (\<forall>bound_nested5.
           ((uniqueIds_op_MessageId_StructAuthorContentQuery (contentQry bound_nested5))
             = (uniqueIds_op_MessageId_MVRegisterQry_String bound_nested5))))"
         
assumes uniqueIds_op_MessageId_RegisterOp_UserId_def:

        "(\<forall>bound_value2.
         ((uniqueIds_op_MessageId_RegisterOp_UserId (Assign_UserId bound_value2))
           = (uniqueIds_op_MessageId_SortCustomUninterpreted_UserId_1 bound_value2)))"
         
assumes uniqueIds_op_MessageId_MVRegisterQry_String_def:

        "(((uniqueIds_op_MessageId_MVRegisterQry_String (ReadFirst_String )) = {})
         \<and> (\<forall>bound_value1.
           ((uniqueIds_op_MessageId_MVRegisterQry_String (MvContains_String bound_value1))
             = (uniqueIds_op_MessageId_SortCustomUninterpreted_String_1 bound_value1))))"
         
assumes uniqueIds_op_MessageId_SortCustomUninterpreted_UserId__def:

        "(\<forall>bound_x13. ((uniqueIds_op_MessageId_SortCustomUninterpreted_UserId_1 bound_x13) = {}))"
         
assumes uniqueIds_op_MessageId_SortCustomUninterpreted_MessageId__def:

        "(\<forall>bound_x12. ((uniqueIds_op_MessageId_SortCustomUninterpreted_MessageId_1 bound_x12) = {bound_x12}))"
         
assumes uniqueIds_op_MessageId_StructAuthorContentOp_def:

        "((\<forall>bound_nested2.
         ((uniqueIds_op_MessageId_StructAuthorContentOp (author bound_nested2))
           = (uniqueIds_op_MessageId_RegisterOp_UserId bound_nested2)))
         \<and> (\<forall>bound_nested3.
           ((uniqueIds_op_MessageId_StructAuthorContentOp (content bound_nested3))
             = (uniqueIds_op_MessageId_RegisterOp_String bound_nested3))))"
         
assumes uniqueIds_op_MessageId_rootCrdtOp_def:

        "((\<forall>bound_nested.
         ((uniqueIds_op_MessageId_rootCrdtOp (chat bound_nested)) = (uniqueIds_op_MessageId_SetOp_MessageId bound_nested)))
         \<and> (\<forall>bound_nested1.
           ((uniqueIds_op_MessageId_rootCrdtOp (message bound_nested1))
             = (uniqueIds_op_MessageId_MapOp_MessageId_StructAuthorContentOp bound_nested1))))"
         
assumes uniqueIds_op_MessageId_RegisterQry_UserId_def:

        "((uniqueIds_op_MessageId_RegisterQry_UserId (ReadRegister_UserId )) = {})"
         
assumes uniqueIds_op_MessageId_SortCustomUninterpreted_String__def:

        "(\<forall>bound_x8. ((uniqueIds_op_MessageId_SortCustomUninterpreted_String_1 bound_x8) = {}))"
         
assumes uniqueIds_op_MessageId_MapOp_MessageId_StructAuthorContentOp_def:

        "((\<forall>bound_key2.
         ((uniqueIds_op_MessageId_MapOp_MessageId_StructAuthorContentOp (DeleteKey_MessageId_StructAuthorContentOp bound_key2))
           = (uniqueIds_op_MessageId_SortCustomUninterpreted_MessageId_1 bound_key2)))
         \<and> (\<forall>bound_key3.
           (\<forall>bound_op.
             ((uniqueIds_op_MessageId_MapOp_MessageId_StructAuthorContentOp (NestedOp_MessageId_StructAuthorContentOp bound_key3
                   bound_op))
               = ((uniqueIds_op_MessageId_SortCustomUninterpreted_MessageId_1 bound_key3) \<union> (uniqueIds_op_MessageId_StructAuthorContentOp bound_op))))))"
         
assumes uniqueIds_op_MessageId_SetOp_MessageId_def:

        "((\<forall>bound_x5.
         ((uniqueIds_op_MessageId_SetOp_MessageId (Add_MessageId bound_x5))
           = (uniqueIds_op_MessageId_SortCustomUninterpreted_MessageId_1 bound_x5)))
         \<and> (\<forall>bound_x6.
           ((uniqueIds_op_MessageId_SetOp_MessageId (Remove_MessageId bound_x6))
             = (uniqueIds_op_MessageId_SortCustomUninterpreted_MessageId_1 bound_x6))))"
         
assumes uniqueIds_op_MessageId_RegisterOp_String_def:

        "(\<forall>bound_value.
         ((uniqueIds_op_MessageId_RegisterOp_String (Assign_String bound_value))
           = (uniqueIds_op_MessageId_SortCustomUninterpreted_String_1 bound_value)))"
         
assumes uniqueIds_op_MessageId_MapQuery_MessageId_StructAuthorContentQuery_def:

        "((\<forall>bound_key.
         ((uniqueIds_op_MessageId_MapQuery_MessageId_StructAuthorContentQuery (ContainsKey_MessageId_StructAuthorContentQuery bound_key))
           = (uniqueIds_op_MessageId_SortCustomUninterpreted_MessageId_1 bound_key)))
         \<and> (\<forall>bound_key1.
           (\<forall>bound_q.
             ((uniqueIds_op_MessageId_MapQuery_MessageId_StructAuthorContentQuery (NestedQuery_MessageId_StructAuthorContentQuery bound_key1
                   bound_q))
               = ((uniqueIds_op_MessageId_SortCustomUninterpreted_MessageId_1 bound_key1) \<union> (uniqueIds_op_MessageId_StructAuthorContentQuery bound_q))))))"
         
assumes uniqueIds_op_MessageId_SetQuery_MessageId_def:

        "(\<forall>bound_x1.
         ((uniqueIds_op_MessageId_SetQuery_MessageId (Contains_MessageId bound_x1))
           = (uniqueIds_op_MessageId_SortCustomUninterpreted_MessageId_1 bound_x1)))"
         
assumes old_transactions_unchanged:

        "(\<forall>c4.
         (\<forall>tx4.
           (((((calls c4) = NoCall) \<and> ((calls1 c4) \<noteq> NoCall)) \<and> ((callOrigin1 c4) = (Some tx4)))
             \<longrightarrow> ((transactionOrigin tx4) = None))))"
         
assumes growth_invocation_res:

        "(\<forall>i1. (((invocationRes i1) \<noteq> NoResult) \<longrightarrow> ((invocationRes1 i1) = (invocationRes i1))))"
         
assumes growth_invocation_op:

        "(\<forall>i.
         ((((invocationOp(currentInvocation := (sendMessage from_init text_init))) i) \<noteq> no_invocation)
           \<longrightarrow> ((invocationOp1 i) = ((invocationOp(currentInvocation := (sendMessage from_init text_init))) i))))"
         
assumes growth_tx_origin:

        "(\<forall>tx3. (\<not>((transactionOrigin tx3) = None) \<longrightarrow> ((transactionOrigin1 tx3) = (transactionOrigin tx3))))"
         
assumes growth_call_tx:

        "(\<forall>c3. (((calls c3) \<noteq> NoCall) \<longrightarrow> ((callOrigin1 c3) = (callOrigin c3))))"
         
assumes growth_happensbefore:

        "(\<forall>c2. (((calls c2) \<noteq> NoCall) \<longrightarrow> ((happensBefore1 c2) = (happensBefore c2))))"
         
assumes growth_calls:

        "(\<forall>c1. (((calls c1) \<noteq> NoCall) \<longrightarrow> ((calls1 c1) = (calls c1))))"
         
assumes growth_callOrigin:

        "(\<forall>c. (\<forall>tx2. (((callOrigin c) = (Some tx2)) \<longrightarrow> ((callOrigin1 c) = (Some tx2)))))"
         
assumes transaction_begin_snapshot_addition_transaction_consistent:

        "(\<forall>bound_c113.
         (\<forall>bound_c213.
           (((bound_c113 \<in> snapshotAddition1) \<and> (bound_c213 \<in> (happensBefore1 bound_c113))) \<longrightarrow> (bound_c213 \<in> snapshotAddition1))))"
         
assumes transaction_begin_snapshot_addition_transaction_consistent_2:

        "(\<forall>bound_c112.
         (\<forall>bound_c212.
           (((bound_c112 \<in> snapshotAddition1) \<and> ((callOrigin1 bound_c112) = (callOrigin1 bound_c212)))
             \<longrightarrow> (bound_c212 \<in> snapshotAddition1))))"
         
assumes transaction_begin_snapshot_addition_subset_calls:

        "(\<forall>bound_c37. ((bound_c37 \<in> snapshotAddition1) \<longrightarrow> ((calls1 bound_c37) \<noteq> NoCall)))"
         
assumes transaction_begin_MessageId_knownIds_are_generated:

        "(\<forall>bound_x29. ((bound_x29 \<in> knownIds_MessageId1) \<longrightarrow> \<not>((generatedIds_MessageId1 bound_x29) = None)))"
         
assumes transaction_begin_call_parameters_generated:

        "(\<forall>bound_c36.
         (\<forall>bound_uid1.
           ((bound_uid1 \<in> (uniqueIds_op_MessageId_SortCall_1 (calls1 bound_c36)))
             \<longrightarrow> ((generatedIds_MessageId1 bound_uid1) \<noteq> None))))"
         
assumes transaction_begin_database_calls_MessageId_are_generated:

        "(\<forall>bound_c35.
         (\<forall>bound_x28.
           ((bound_x28 \<in> (uniqueIds_op_MessageId_SortCall_1 (calls1 bound_c35))) \<longrightarrow> ((generatedIds_MessageId1 bound_x28) \<noteq> None))))"
         
assumes transaction_begin_sendMessage_MessageId_res_known:

        "(\<forall>bound_i11. ((uniqueIds_op_MessageId_SortInvocationRes_1 (invocationRes1 bound_i11)) \<subseteq> knownIds_MessageId1))"
         
assumes transaction_begin_sendMessage_MessageId_args_known:

        "(\<forall>bound_i10. ((uniqueIds_op_MessageId_SortInvocationInfo_1 (invocationOp1 bound_i10)) \<subseteq> knownIds_MessageId1))"
         
assumes transaction_begin_WF_transactionOrigin_exists:

        "(\<forall>bound_tx10.
         (\<forall>bound_i9. (((transactionOrigin1 bound_tx10) = (Some bound_i9)) \<longrightarrow> ((invocationOp1 bound_i9) \<noteq> no_invocation))))"
         
assumes transaction_begin_WF_callOrigin_exists:

        "(\<forall>bound_ca1. (\<forall>bound_tx9. (((callOrigin1 bound_ca1) = (Some bound_tx9)) \<longrightarrow> \<not>((transactionOrigin1 bound_tx9) = None))))"
         
assumes transaction_begin_WF_no_call_implies_no_happensBefore:

        "(\<forall>bound_c34. (((calls1 bound_c34) = NoCall) \<longrightarrow> ((happensBefore1 bound_c34) = {})))"
         
assumes transaction_begin_WF_transactionOrigin_callOrigin:

        "(\<forall>bound_tx8. (((transactionOrigin1 bound_tx8) = None) \<longrightarrow> (\<forall>bound_c33. ((callOrigin1 bound_c33) \<noteq> (Some bound_tx8)))))"
         
assumes transaction_begin_WF_callOrigin:

        "(\<forall>bound_c32. (((callOrigin1 bound_c32) = None) = ((calls1 bound_c32) = NoCall)))"
         
assumes transaction_begin_WF_invocationCalls:

        "(\<forall>bound_i8.
         (\<forall>bound_c31.
           ((bound_c31 \<in> (invocationCalls1 bound_i8))
             = (\<exists>bound_tx7. (((callOrigin1 bound_c31) = (Some bound_tx7)) \<and> ((transactionOrigin1 bound_tx7) = (Some bound_i8)))))))"
         
assumes transaction_begin_happens_before_transaction_consistent_r:

        "(\<forall>bound_x27.
         (\<forall>bound_y14.
           (\<forall>bound_y24.
             (((((callOrigin1 bound_y14) = (callOrigin1 bound_y24)) \<and> \<not>((callOrigin1 bound_x27) = (callOrigin1 bound_y14)))
               \<and> (bound_x27 \<in> (happensBefore1 bound_y14)))
               \<longrightarrow> (bound_x27 \<in> (happensBefore1 bound_y24))))))"
         
assumes transaction_begin_happens_before_transaction_consistent_l:

        "(\<forall>bound_x26.
         (\<forall>bound_y13.
           (\<forall>bound_y23.
             (((((callOrigin1 bound_y13) = (callOrigin1 bound_y23)) \<and> \<not>((callOrigin1 bound_x26) = (callOrigin1 bound_y13)))
               \<and> (bound_y13 \<in> (happensBefore1 bound_x26)))
               \<longrightarrow> (bound_y23 \<in> (happensBefore1 bound_x26))))))"
         
assumes transaction_begin_no_invocation_implies_no_result:

        "(\<forall>bound_i7. (((invocationOp1 bound_i7) = no_invocation) \<longrightarrow> ((invocationRes1 bound_i7) = NoResult)))"
         
assumes transaction_begin_happensBefore_antisym:

        "(\<forall>bound_x25. (\<forall>bound_y4. ((bound_x25 \<in> (happensBefore1 bound_y4)) \<longrightarrow> \<not>(bound_y4 \<in> (happensBefore1 bound_x25)))))"
         
assumes transaction_begin_happensBefore_trans:

        "(\<forall>bound_x24.
         (\<forall>bound_y3.
           (\<forall>bound_z1.
             (((bound_x24 \<in> (happensBefore1 bound_y3)) \<and> (bound_y3 \<in> (happensBefore1 bound_z1)))
               \<longrightarrow> (bound_x24 \<in> (happensBefore1 bound_z1))))))"
         
assumes transaction_begin_happensBefore_non_reflex:

        "(\<forall>bound_c30. \<not>(bound_c30 \<in> (happensBefore1 bound_c30)))"
         
assumes transaction_begin_visibleCalls_causally_consistent:

        "(\<forall>bound_c111. (\<forall>bound_c211. (((bound_c211 \<in> vis) \<and> (bound_c111 \<in> (happensBefore1 bound_c211))) \<longrightarrow> (bound_c111 \<in> vis))))"
         
assumes transaction_begin_visibleCalls_transaction_consistent1:

        "(\<forall>bound_c110.
         (\<forall>bound_c210.
           ((((bound_c110 \<in> vis) \<and> ((callOrigin1 bound_c110) = (callOrigin1 bound_c210))) \<and> ((calls1 bound_c210) \<noteq> NoCall))
             \<longrightarrow> (bound_c210 \<in> vis))))"
         
assumes transaction_begin_visibleCalls_exist:

        "(\<forall>bound_c20. ((bound_c20 \<in> vis) \<longrightarrow> ((calls1 bound_c20) \<noteq> NoCall)))"
         
assumes transaction_begin_invocation_sequential:

        "(\<forall>bound_c19.
         (\<forall>bound_tx11.
           (\<forall>bound_i6.
             (\<forall>bound_c29.
               (\<forall>bound_tx21.
                 (((((((callOrigin1 bound_c19) = (Some bound_tx11)) \<and> ((transactionOrigin1 bound_tx11) = (Some bound_i6)))
                   \<and> ((callOrigin1 bound_c29) = (Some bound_tx21)))
                   \<and> ((transactionOrigin1 bound_tx21) = (Some bound_i6)))
                   \<and> (bound_c19 \<noteq> bound_c29))
                   \<longrightarrow> ((bound_c19 \<in> (happensBefore1 bound_c29)) \<or> (bound_c29 \<in> (happensBefore1 bound_c19)))))))))"
         
assumes transaction_begin_happensBefore_exists_r:

        "(\<forall>bound_c18. (\<forall>bound_c28. ((bound_c18 \<in> (happensBefore1 bound_c28)) \<longrightarrow> ((calls1 bound_c28) \<noteq> NoCall))))"
         
assumes transaction_begin_happensBefore_exists_l:

        "(\<forall>bound_c17. (\<forall>bound_c27. ((bound_c17 \<in> (happensBefore1 bound_c27)) \<longrightarrow> ((calls1 bound_c17) \<noteq> NoCall))))"
         
assumes at_transaction_begin_invariant_inv0:

        "\<not>(\<exists>bound_write2.
         (\<exists>bound_delete2.
           (\<exists>bound_m2.
             (\<exists>bound_upd2.
               ((((calls1 bound_write2) = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_m2 bound_upd2))))
                 \<and> ((calls1 bound_delete2) = (Op (message (DeleteKey_MessageId_StructAuthorContentOp bound_m2)))))
                 \<and> (bound_delete2 \<in> (happensBefore1 bound_write2)))))))"
         
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

        "((generatedIds_MessageId1 m) = None)"
         
assumes c0_freshB:

        "((calls1 c0) = NoCall)"
         
assumes c0_freshA:

        "distinct [c0]"
         
assumes calls_def:

        "(calls2
         = (calls1(c0 := (Op (message (NestedOp_MessageId_StructAuthorContentOp m (author (Assign_UserId from_init))))))))"
         
assumes c11_freshB:

        "((calls2 c11) = NoCall)"
         
assumes c11_freshA:

        "distinct [c11 , c0]"
         
assumes calls_def_2:

        "(calls3
         = (calls2(c11 := (Op (message (NestedOp_MessageId_StructAuthorContentOp m (content (Assign_String text_init))))))))"
         
assumes c21_freshB:

        "((calls3 c21) = NoCall)"
         
assumes c21_freshA:

        "distinct [c21 , c0 , c11]"
         
assumes calls_def_3:

        "(calls4 = (calls3(c21 := (Op (chat (Add_MessageId m))))))"
         
assumes vis_def:

        "(vis1 = (vis \<union> {c0}))"
         
assumes vis_def_2:

        "(vis2 = (vis1 \<union> {c11}))"
         
assumes happensBefore_def:

        "(happensBefore2 = (happensBefore1(c0 := vis)))"
         
assumes happensBefore_def_2:

        "(happensBefore3 = (happensBefore2(c11 := vis1)))"
         
assumes happensBefore_def_3:

        "(happensBefore4 = (happensBefore3(c21 := vis2)))"
         
assumes invariant_not_violated:

        "\<not>\<not>(\<exists>bound_write3.
         (\<exists>bound_delete3.
           (\<exists>bound_m3.
             (\<exists>bound_upd3.
               ((((calls4 bound_write3) = (Op (message (NestedOp_MessageId_StructAuthorContentOp bound_m3 bound_upd3))))
                 \<and> ((calls4 bound_delete3) = (Op (message (DeleteKey_MessageId_StructAuthorContentOp bound_m3)))))
                 \<and> (bound_delete3 \<in> (happensBefore4 bound_write3)))))))"
         shows False
  using invariant_not_violated
proof auto
  fix bound_write3 bound_delete3 bound_m3 bound_upd3
  assume a0: "calls4 bound_delete3 = Op (message (DeleteKey_MessageId_StructAuthorContentOp bound_m3))"
    and a1: "bound_delete3 \<in> happensBefore4 bound_write3"
    and a2: "calls4 bound_write3 = Op (message (NestedOp_MessageId_StructAuthorContentOp bound_m3 bound_upd3))"


  from at_transaction_begin_invariant_inv0
  have True by simp

(*
  have "bound_write3 \<noteq> c0"
    using c0_freshB a2
    unfolding calls_def_3 calls_def_2 calls_def
    apply (auto split: if_splits)
      (* TODO by unique id *)
    sorry
*)

  \<comment> \<open>There cannot be any database call yet for the new unique id\<close>
  have "m \<notin> uniqueIds_op_MessageId_SortCall_1 (calls1 bound_delete3)"
  proof 
    assume a: "m \<in> uniqueIds_op_MessageId_SortCall_1 (calls1 bound_delete3)"
    from transaction_begin_database_calls_MessageId_are_generated[rule_format, OF a]
    have "generatedIds_MessageId1 m \<noteq> None" .

    thus False
      by (simp add: m_new_id_fresh)
  qed

  have "m \<in> uniqueIds_op_MessageId_SortCall_1 (Op (message (DeleteKey_MessageId_StructAuthorContentOp m)))"
    find_theorems uniqueIds_op_MessageId_SortCall_1
    sorry
  hence "calls1 bound_delete3 \<noteq> Op (message (DeleteKey_MessageId_StructAuthorContentOp m))"

    using transaction_begin_database_calls_MessageId_are_generated[rule_format]

    find_theorems
    sorry
    find_theorems name: unique name: "local."


  from a0 a1 a2
  have undefined
    unfolding calls_def_3 calls_def_2 calls_def
      happensBefore_def_3 happensBefore_def_2 happensBefore_def
    apply (auto split: if_splits)
    subgoal
      (* bound_delete3 \<in> vis1 *)
      (* TODO unique id database *)
      sorry
    subgoal
      sorry
    subgoal
      using at_transaction_begin_invariant_inv0 by blast
    done

    apply (auto simp add: calls_def_3 )

  find_theorems calls4


  show "False"


