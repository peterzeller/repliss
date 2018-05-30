theory chatappdtrpls_CrdtProgram_WP_parameter_sendMessage_1
  imports Why3
begin

why3_open "chatappdtrpls_CrdtProgram_WP_parameter_sendMessage_1.xml"

why3_vc WP_parameter_sendMessage
proof (auto simp add: message_exists_def)

  show " \<exists>c1. (state_visibleCalls5 c1 \<and> (\<exists>args. state_callOps5 c1 = Op_message_author_assign m args) \<or>
          state_visibleCalls5 c1 \<and> (\<exists>args. state_callOps5 c1 = Op_message_content_assign m args) \<or>
          state_visibleCalls5 c1 \<and> (\<exists>args. state_callOps5 c1 = Op_message_chat_assign m args)) \<and>
         (\<forall>c2. state_visibleCalls5 c2 \<and> state_callOps5 c2 = Op_message_delete m \<longrightarrow> state_happensBefore5 c1 c2)"
  proof (cases "o1 = m")
    case True

    have us2_op: "state_callOps5 (CallId us2) = Op_message_chat_assign m toC"
      by (metis H78 H86 H88 True operation.distinct(11))

    have us2_vis: "state_visibleCalls5 (CallId us2)"
      by (simp add: H81 H90)

    show " \<exists>c1. (state_visibleCalls5 c1 \<and> (\<exists>args. state_callOps5 c1 = Op_message_author_assign m args) \<or>
          state_visibleCalls5 c1 \<and> (\<exists>args. state_callOps5 c1 = Op_message_content_assign m args) \<or>
          state_visibleCalls5 c1 \<and> (\<exists>args. state_callOps5 c1 = Op_message_chat_assign m args)) \<and>
         (\<forall>c2. state_visibleCalls5 c2 \<and> state_callOps5 c2 = Op_message_delete m \<longrightarrow> state_happensBefore5 c1 c2)"
    proof 
      show "  (state_visibleCalls5 (CallId us2) \<and> (\<exists>args. state_callOps5 (CallId us2) = Op_message_author_assign m args) \<or>
          state_visibleCalls5 (CallId us2) \<and> (\<exists>args. state_callOps5 (CallId us2) = Op_message_content_assign m args) \<or>
          state_visibleCalls5 (CallId us2) \<and> (\<exists>args. state_callOps5 (CallId us2) = Op_message_chat_assign m args)) \<and>
         (\<forall>c2. state_visibleCalls5 c2 \<and> state_callOps5 c2 = Op_message_delete m \<longrightarrow> state_happensBefore5 (CallId us2) c2)"
        apply (auto simp add: us2_op us2_vis)
        using H80 H87 H89 H90 by auto

    qed
      next
        case False

        have c_contains_m_new:
    "chat_messages_contains c m \<lparr>contents = state_locallyGenerated_messageId(o1 := True)\<rparr> \<lparr>contents = state_exposed_messageId\<rparr> \<lparr>contents = state_knownIds_messageId\<rparr>
     \<lparr>contents = state_callOps5\<rparr> \<lparr>contents = state_visibleCalls5\<rparr> \<lparr>contents = state_happensBefore5\<rparr> \<lparr>contents = state_sameTransaction5\<rparr> \<lparr>contents = state_currentTransaction5\<rparr>
     \<lparr>contents = state_origin5\<rparr> \<lparr>contents = state_invocations2\<rparr> \<lparr>contents = state_invocationResult1\<rparr> \<lparr>contents = state_invocationHappensBefore1\<rparr>"
          using H95 by blast

        from c_contains_m_new
        have c_contains_m_old:
            "chat_messages_contains c m \<lparr>contents = state_locallyGenerated_messageId\<rparr> \<lparr>contents = state_exposed_messageId\<rparr> \<lparr>contents = state_knownIds_messageId\<rparr>
           \<lparr>contents = state_callOps1\<rparr> \<lparr>contents = state_visibleCalls1\<rparr> \<lparr>contents = state_happensBefore1\<rparr> \<lparr>contents = state_sameTransaction1\<rparr> \<lparr>contents = state_currentTransaction1\<rparr>
           \<lparr>contents = state_origin1\<rparr> \<lparr>contents = state_invocations2\<rparr> \<lparr>contents = state_invocationResult1\<rparr> \<lparr>contents = state_invocationHappensBefore1\<rparr>"
          apply (auto simp add: chat_messages_contains_def)
          find_theorems state_callOps5
          apply (auto simp add: state_callOps5)



        have "chat_messages_contains c m \<lparr>contents = state_locallyGenerated_messageId\<rparr> \<lparr>contents = state_exposed_messageId\<rparr> \<lparr>contents = state_knownIds_messageId\<rparr>
           \<lparr>contents = state_callOps1\<rparr> \<lparr>contents = state_visibleCalls1\<rparr> \<lparr>contents = state_happensBefore1\<rparr> \<lparr>contents = state_sameTransaction1\<rparr> \<lparr>contents = state_currentTransaction1\<rparr>
           \<lparr>contents = state_origin1\<rparr> \<lparr>contents = state_invocations2\<rparr> \<lparr>contents = state_invocationResult1\<rparr> \<lparr>contents = state_invocationHappensBefore1\<rparr>
 \<longrightarrow>
          message_exists m \<lparr>contents = state_locallyGenerated_messageId\<rparr> \<lparr>contents = state_exposed_messageId\<rparr> \<lparr>contents = state_knownIds_messageId\<rparr> \<lparr>contents = state_callOps1\<rparr>
           \<lparr>contents = state_visibleCalls1\<rparr> \<lparr>contents = state_happensBefore1\<rparr> \<lparr>contents = state_sameTransaction1\<rparr> \<lparr>contents = state_currentTransaction1\<rparr> \<lparr>contents = state_origin1\<rparr>
           \<lparr>contents = state_invocations2\<rparr> \<lparr>contents = state_invocationResult1\<rparr> \<lparr>contents = state_invocationHappensBefore1\<rparr>"
          using H31 by simp




        find_theorems chat_messages_contains

        then show ?thesis sorry
      qed
      find_theorems message_exists
why3_end

end
