
type {:datatype } userRecordField;

function {:constructor } f_mail(): userRecordField;

function {:constructor } f_name(): userRecordField;

function {:constructor } f_id(): userRecordField;

type {:datatype } getUserResult;

function {:constructor } found(name: String, mail: String): getUserResult;

function {:constructor } notFound(): getUserResult;

type {:datatype } operation;

function {:constructor } queryop_mapGet(u: UserId, f: userRecordField, result: String): operation;

function {:constructor } queryop_mapExists(u: UserId, result: bool): operation;

function {:constructor } mapDelete(uid: UserId): operation;

function {:constructor } mapWrite(uid: UserId, field: userRecordField, value: String): operation;

function {:constructor } noop(): operation;

type {:datatype } callId;

function {:constructor } CallId(id: int): callId;

type UserId;

type {:datatype } invocationId;

function {:constructor } InvocationId(id: int): invocationId;

type {:datatype } invocationInfo;

function {:constructor } invocation_getUser(id: UserId, result: getUserResult): invocationInfo;

function {:constructor } invocation_removeUser(id: UserId): invocationInfo;

function {:constructor } invocation_updateMail(id: UserId, newMail: String): invocationInfo;

function {:constructor } invocation_registerUser(name: String, mail: String, result: UserId): invocationInfo;

function {:constructor } NoInvocation(): invocationInfo;

type String;
var state_knownIds_UserId: [UserId]bool;
var state_callOps: [callId]operation;
var state_visibleCalls: [callId]bool;
var state_happensBefore: [callId, callId]bool;
var state_sameTransaction: [callId, callId]bool;
var state_currentTransaction: [callId]bool;
var state_maxId: int;
var state_origin: [callId]invocationId;
var state_inCurrentInvocation: [callId]bool;
var state_invocations: [invocationId]invocationInfo;
var state_invocationHappensBefore: [invocationId, invocationId]bool;

function mapExists(u: UserId, state_knownIds_UserId: [UserId]bool, state_callOps: [callId]operation, state_visibleCalls: [callId]bool, state_happensBefore: [callId, callId]bool, state_sameTransaction: [callId, callId]bool, state_currentTransaction: [callId]bool, state_maxId: int, state_origin: [callId]invocationId, state_inCurrentInvocation: [callId]bool, state_invocations: [invocationId]invocationInfo, state_invocationHappensBefore: [invocationId, invocationId]bool): bool{
(exists c1: callId, f: userRecordField, v: String :: ((state_visibleCalls[c1] && (state_callOps[c1] == mapWrite(u, f, v))) && (forall c2: callId :: ((state_visibleCalls[c2] && (state_callOps[c2] == mapDelete(u))) ==> state_happensBefore[c2, c1]))))}

function mapGet(u: UserId, f: userRecordField, state_knownIds_UserId: [UserId]bool, state_callOps: [callId]operation, state_visibleCalls: [callId]bool, state_happensBefore: [callId, callId]bool, state_sameTransaction: [callId, callId]bool, state_currentTransaction: [callId]bool, state_maxId: int, state_origin: [callId]invocationId, state_inCurrentInvocation: [callId]bool, state_invocations: [invocationId]invocationInfo, state_invocationHappensBefore: [invocationId, invocationId]bool): String;

function WellFormed(state_knownIds_UserId: [UserId]bool, state_callOps: [callId]operation, state_visibleCalls: [callId]bool, state_happensBefore: [callId, callId]bool, state_sameTransaction: [callId, callId]bool, state_currentTransaction: [callId]bool, state_maxId: int, state_origin: [callId]invocationId, state_inCurrentInvocation: [callId]bool, state_invocations: [invocationId]invocationInfo, state_invocationHappensBefore: [invocationId, invocationId]bool): bool{
(((((((forall c1: callId, c2: callId :: (((state_callOps[c1] == noop()) || (state_callOps[c2] == noop())) ==> !(state_happensBefore[c1, c2]))) && (forall c: callId :: (state_visibleCalls[c] ==> (state_callOps[c] != noop())))) && (forall c: callId :: ((state_callOps[c] != noop()) ==> state_happensBefore[c, c]))) && (forall x: callId, y: callId, z: callId :: ((state_happensBefore[x, y] && state_happensBefore[y, z]) ==> state_happensBefore[x, z]))) && (forall x: callId, y: callId :: ((state_happensBefore[x, y] && state_happensBefore[y, x]) ==> (x == y)))) && (forall i: int :: ((i >= state_maxId) ==> (state_callOps[CallId(i)] == noop())))) && (forall c1: callId, c2: callId :: ((((state_callOps[c1] != noop()) && (state_callOps[c2] != noop())) && state_invocationHappensBefore[state_origin[c1], state_origin[c2]]) ==> state_happensBefore[c1, c2])))}

procedure beginAtomic()
modifies state_visibleCalls;
free ensures WellFormed(state_knownIds_UserId, state_callOps, state_visibleCalls, state_happensBefore, state_sameTransaction, state_currentTransaction, state_maxId, state_origin, state_inCurrentInvocation, state_invocations, state_invocationHappensBefore);
free ensures (forall c: callId :: (old(state_visibleCalls[c]) ==> state_visibleCalls[c]));
free ensures (forall c1: callId, c2: callId :: ((state_visibleCalls[c2] && state_happensBefore[c1, c2]) ==> state_visibleCalls[c1]));
free ensures (forall c1: callId, c2: callId :: ((state_visibleCalls[c1] && state_sameTransaction[c1, c2]) ==> state_visibleCalls[c2]));
{
}

procedure endAtomic()
requires (forall r: invocationId, g: invocationId, u: UserId, res: getUserResult :: ((((state_invocations[r] == invocation_removeUser(u)) && (state_invocations[g] == invocation_getUser(u, res))) && state_invocationHappensBefore[r, g]) ==> (res == notFound())));
requires (forall u: UserId, i: invocationId :: ((state_invocations[i] == invocation_removeUser(u)) ==> (exists c: callId :: ((state_origin[c] == i) && (state_callOps[c] == mapDelete(u))))));
requires (forall c1: callId, c2: callId, u: UserId, f: userRecordField, v: String :: (((state_callOps[c1] == mapDelete(u)) && (state_callOps[c2] == mapWrite(u, f, v))) ==> !(state_happensBefore[c1, c2])));
{
}

procedure crdtOperation(operation: operation)
modifies state_callOps, state_happensBefore, state_visibleCalls, state_sameTransaction, state_currentTransaction, state_maxId, state_inCurrentInvocation;
requires WellFormed(state_knownIds_UserId, state_callOps, state_visibleCalls, state_happensBefore, state_sameTransaction, state_currentTransaction, state_maxId, state_origin, state_inCurrentInvocation, state_invocations, state_invocationHappensBefore);
free ensures (old(state_callOps[CallId((old(state_maxId) + 1))]) == noop());
free ensures (state_callOps[CallId((old(state_maxId) + 1))] == operation);
free ensures (forall c1: callId :: ((c1 != CallId((old(state_maxId) + 1))) ==> (state_callOps[c1] == old(state_callOps[c1]))));
free ensures (forall c1: callId, c2: callId :: (state_happensBefore[c1, c2] <==> (old(state_happensBefore[c1, c2]) || ((state_visibleCalls[c1] || (c1 == c2)) && (c2 == CallId((old(state_maxId) + 1)))))));
free ensures (forall c1: callId :: (state_visibleCalls[c1] <==> (old(state_visibleCalls[c1]) || (c1 == CallId((old(state_maxId) + 1))))));
free ensures (forall c1: callId :: (state_inCurrentInvocation[c1] == old((state_inCurrentInvocation[c1] || (c1 == CallId((old(state_maxId) + 1)))))));
free ensures WellFormed(state_knownIds_UserId, state_callOps, state_visibleCalls, state_happensBefore, state_sameTransaction, state_currentTransaction, state_maxId, state_origin, state_inCurrentInvocation, state_invocations, state_invocationHappensBefore);
{
}

procedure finishInvocation(invocation: invocationInfo) returns (newInvocId: invocationId)
modifies state_origin, state_invocations, state_invocationHappensBefore, state_inCurrentInvocation;
requires WellFormed(state_knownIds_UserId, state_callOps, state_visibleCalls, state_happensBefore, state_sameTransaction, state_currentTransaction, state_maxId, state_origin, state_inCurrentInvocation, state_invocations, state_invocationHappensBefore);
free ensures (forall c: callId :: (old(state_inCurrentInvocation[c]) ==> (state_origin[c] == newInvocId)));
free ensures (forall c: callId :: (!(old(state_inCurrentInvocation[c])) ==> (state_origin[c] == old(state_origin[c]))));
free ensures old((state_invocations[newInvocId] == NoInvocation()));
free ensures (state_invocations[newInvocId] == invocation);
free ensures (forall i: invocationId :: ((i != newInvocId) ==> (state_invocations[i] == old(state_invocations[i]))));
free ensures (forall i: invocationId :: old(!(state_invocationHappensBefore[i, newInvocId])));
free ensures (forall i: invocationId :: old(!(state_invocationHappensBefore[newInvocId, i])));
free ensures (forall c: callId :: !(state_inCurrentInvocation[c]));
free ensures (forall i1: invocationId, i2: invocationId :: (state_invocationHappensBefore[i1, i2] == (old(state_invocationHappensBefore[i1, i2]) || ((((i2 == newInvocId) && (exists c: callId :: old(state_inCurrentInvocation[c]))) && (exists c: callId :: (state_origin[c] == i1))) && (forall c1: callId, c2: callId :: (((state_origin[c1] == i1) && old(state_inCurrentInvocation[c2])) ==> state_happensBefore[c1, c2]))))));
{
}

procedure check_initialState()
requires (forall c: callId :: (state_callOps[c] == noop()));
requires (forall c: callId :: !(state_visibleCalls[c]));
requires (forall c1: callId, c2: callId :: !(state_happensBefore[c1, c2]));
requires (forall c1: callId, c2: callId :: !(state_sameTransaction[c1, c2]));
requires (forall c: callId :: !(state_currentTransaction[c]));
requires (forall c: callId :: !(state_inCurrentInvocation[c]));
requires (forall i: invocationId :: (state_invocations[i] == NoInvocation()));
requires (forall i1: invocationId, i2: invocationId :: !(state_invocationHappensBefore[i1, i2]));
ensures WellFormed(state_knownIds_UserId, state_callOps, state_visibleCalls, state_happensBefore, state_sameTransaction, state_currentTransaction, state_maxId, state_origin, state_inCurrentInvocation, state_invocations, state_invocationHappensBefore);
ensures (forall r: invocationId, g: invocationId, u: UserId, res: getUserResult :: ((((state_invocations[r] == invocation_removeUser(u)) && (state_invocations[g] == invocation_getUser(u, res))) && state_invocationHappensBefore[r, g]) ==> (res == notFound())));
ensures (forall u: UserId, i: invocationId :: ((state_invocations[i] == invocation_removeUser(u)) ==> (exists c: callId :: ((state_origin[c] == i) && (state_callOps[c] == mapDelete(u))))));
ensures (forall c1: callId, c2: callId, u: UserId, f: userRecordField, v: String :: (((state_callOps[c1] == mapDelete(u)) && (state_callOps[c2] == mapWrite(u, f, v))) ==> !(state_happensBefore[c1, c2])));
{
}

procedure registerUser(name: String, mail: String) returns (result: UserId)
modifies state_knownIds_UserId, state_callOps, state_visibleCalls, state_happensBefore, state_sameTransaction, state_currentTransaction, state_maxId, state_origin, state_inCurrentInvocation, state_invocations, state_invocationHappensBefore;
requires WellFormed(state_knownIds_UserId, state_callOps, state_visibleCalls, state_happensBefore, state_sameTransaction, state_currentTransaction, state_maxId, state_origin, state_inCurrentInvocation, state_invocations, state_invocationHappensBefore);
requires (forall c: callId :: !(state_inCurrentInvocation[c]));
requires (forall r: invocationId, g: invocationId, u: UserId, res: getUserResult :: ((((state_invocations[r] == invocation_removeUser(u)) && (state_invocations[g] == invocation_getUser(u, res))) && state_invocationHappensBefore[r, g]) ==> (res == notFound())));
requires (forall u: UserId, i: invocationId :: ((state_invocations[i] == invocation_removeUser(u)) ==> (exists c: callId :: ((state_origin[c] == i) && (state_callOps[c] == mapDelete(u))))));
requires (forall c1: callId, c2: callId, u: UserId, f: userRecordField, v: String :: (((state_callOps[c1] == mapDelete(u)) && (state_callOps[c2] == mapWrite(u, f, v))) ==> !(state_happensBefore[c1, c2])));
ensures (forall r: invocationId, g: invocationId, u: UserId, res: getUserResult :: ((((state_invocations[r] == invocation_removeUser(u)) && (state_invocations[g] == invocation_getUser(u, res))) && state_invocationHappensBefore[r, g]) ==> (res == notFound())));
ensures (forall u: UserId, i: invocationId :: ((state_invocations[i] == invocation_removeUser(u)) ==> (exists c: callId :: ((state_origin[c] == i) && (state_callOps[c] == mapDelete(u))))));
ensures (forall c1: callId, c2: callId, u: UserId, f: userRecordField, v: String :: (((state_callOps[c1] == mapDelete(u)) && (state_callOps[c2] == mapWrite(u, f, v))) ==> !(state_happensBefore[c1, c2])));
{
  var u: UserId;
  var newInvocationId: invocationId;
  assume {:captureState "[line 44:0] start of procedure registerUser"} true;
  assume {:captureState "[line 44:53] "} true;
  assume {:captureState "[line 46:2] "} true;
  havoc u;
  assume (forall c: callId, _p_u: UserId, _p_f: userRecordField, _p_result: String :: ((state_callOps[c] == queryop_mapGet(_p_u, _p_f, _p_result)) ==> (u != _p_u)));
  assume (forall c: callId, _p_u: UserId, _p_result: bool :: ((state_callOps[c] == queryop_mapExists(_p_u, _p_result)) ==> (u != _p_u)));
  assume (forall c: callId, _p_uid: UserId :: ((state_callOps[c] == mapDelete(_p_uid)) ==> (u != _p_uid)));
  assume (forall c: callId, _p_uid: UserId, _p_field: userRecordField, _p_value: String :: ((state_callOps[c] == mapWrite(_p_uid, _p_field, _p_value)) ==> (u != _p_uid)));
  assume {:captureState "[line 47:2] "} true;
  call beginAtomic();
  assume {:captureState "[line 47:2] begin atomic"} true;
  assume {:captureState "[line 47:9] "} true;
  assume {:captureState "[line 48:4] "} true;
  call crdtOperation(mapWrite(u, f_name(), name));
  assume {:captureState "[line 49:4] "} true;
  call crdtOperation(mapWrite(u, f_mail(), mail));
  assume {:captureState "[line 47:2] before commit"} true;
  call endAtomic();
  assume {:captureState "[line 50:3] end atomic"} true;
  assume {:captureState "[line 51:2] "} true;
  assume {:captureState "[line 51:2] before return"} true;
  call newInvocationId := finishInvocation(invocation_registerUser(name, mail, u));
  result := u; return;
  assume {:captureState "[line 52:1] end of procedure registerUser"} true;
}

procedure updateMail(id: UserId, newMail: String)
modifies state_knownIds_UserId, state_callOps, state_visibleCalls, state_happensBefore, state_sameTransaction, state_currentTransaction, state_maxId, state_origin, state_inCurrentInvocation, state_invocations, state_invocationHappensBefore;
requires WellFormed(state_knownIds_UserId, state_callOps, state_visibleCalls, state_happensBefore, state_sameTransaction, state_currentTransaction, state_maxId, state_origin, state_inCurrentInvocation, state_invocations, state_invocationHappensBefore);
requires (forall c: callId :: !(state_inCurrentInvocation[c]));
requires (forall r: invocationId, g: invocationId, u: UserId, res: getUserResult :: ((((state_invocations[r] == invocation_removeUser(u)) && (state_invocations[g] == invocation_getUser(u, res))) && state_invocationHappensBefore[r, g]) ==> (res == notFound())));
requires (forall u: UserId, i: invocationId :: ((state_invocations[i] == invocation_removeUser(u)) ==> (exists c: callId :: ((state_origin[c] == i) && (state_callOps[c] == mapDelete(u))))));
requires (forall c1: callId, c2: callId, u: UserId, f: userRecordField, v: String :: (((state_callOps[c1] == mapDelete(u)) && (state_callOps[c2] == mapWrite(u, f, v))) ==> !(state_happensBefore[c1, c2])));
ensures (forall r: invocationId, g: invocationId, u: UserId, res: getUserResult :: ((((state_invocations[r] == invocation_removeUser(u)) && (state_invocations[g] == invocation_getUser(u, res))) && state_invocationHappensBefore[r, g]) ==> (res == notFound())));
ensures (forall u: UserId, i: invocationId :: ((state_invocations[i] == invocation_removeUser(u)) ==> (exists c: callId :: ((state_origin[c] == i) && (state_callOps[c] == mapDelete(u))))));
ensures (forall c1: callId, c2: callId, u: UserId, f: userRecordField, v: String :: (((state_callOps[c1] == mapDelete(u)) && (state_callOps[c2] == mapWrite(u, f, v))) ==> !(state_happensBefore[c1, c2])));
{
  var uExists: bool;
  var __query_1: bool;
  var newInvocationId: invocationId;
  assume {:captureState "[line 54:0] start of procedure updateMail"} true;
  assume {:captureState "[line 54:44] "} true;
  assume {:captureState "[line 56:2] "} true;
  call beginAtomic();
  assume {:captureState "[line 56:2] begin atomic"} true;
  assume {:captureState "[line 56:9] "} true;
  assume {:captureState "[line 57:14] "} true;
  __query_1 := mapExists(id, state_knownIds_UserId, state_callOps, state_visibleCalls, state_happensBefore, state_sameTransaction, state_currentTransaction, state_maxId, state_origin, state_inCurrentInvocation, state_invocations, state_invocationHappensBefore);
  assume {:captureState "[line 57:14] "} true;
  call crdtOperation(queryop_mapExists(id, __query_1));
  assume {:captureState "[line 57:4] "} true;
  uExists := __query_1;
  assume {:captureState "[line 58:4] "} true;
  if (uExists) {
    assume {:captureState "[line 58:17] "} true;
    assume {:captureState "[line 59:6] "} true;
    call crdtOperation(mapWrite(id, f_mail(), newMail));
  } else {
    assume {:captureState "[line 0:0] "} true;
  }
  assume {:captureState "[line 56:2] before commit"} true;
  call endAtomic();
  assume {:captureState "[line 61:3] end atomic"} true;
  assume {:captureState "[line 54:0] before return"} true;
  call newInvocationId := finishInvocation(invocation_updateMail(id, newMail));
  assume {:captureState "[line 62:1] end of procedure updateMail"} true;
}

procedure removeUser(id: UserId)
modifies state_knownIds_UserId, state_callOps, state_visibleCalls, state_happensBefore, state_sameTransaction, state_currentTransaction, state_maxId, state_origin, state_inCurrentInvocation, state_invocations, state_invocationHappensBefore;
requires WellFormed(state_knownIds_UserId, state_callOps, state_visibleCalls, state_happensBefore, state_sameTransaction, state_currentTransaction, state_maxId, state_origin, state_inCurrentInvocation, state_invocations, state_invocationHappensBefore);
requires (forall c: callId :: !(state_inCurrentInvocation[c]));
requires (forall r: invocationId, g: invocationId, u: UserId, res: getUserResult :: ((((state_invocations[r] == invocation_removeUser(u)) && (state_invocations[g] == invocation_getUser(u, res))) && state_invocationHappensBefore[r, g]) ==> (res == notFound())));
requires (forall u: UserId, i: invocationId :: ((state_invocations[i] == invocation_removeUser(u)) ==> (exists c: callId :: ((state_origin[c] == i) && (state_callOps[c] == mapDelete(u))))));
requires (forall c1: callId, c2: callId, u: UserId, f: userRecordField, v: String :: (((state_callOps[c1] == mapDelete(u)) && (state_callOps[c2] == mapWrite(u, f, v))) ==> !(state_happensBefore[c1, c2])));
ensures (forall r: invocationId, g: invocationId, u: UserId, res: getUserResult :: ((((state_invocations[r] == invocation_removeUser(u)) && (state_invocations[g] == invocation_getUser(u, res))) && state_invocationHappensBefore[r, g]) ==> (res == notFound())));
ensures (forall u: UserId, i: invocationId :: ((state_invocations[i] == invocation_removeUser(u)) ==> (exists c: callId :: ((state_origin[c] == i) && (state_callOps[c] == mapDelete(u))))));
ensures (forall c1: callId, c2: callId, u: UserId, f: userRecordField, v: String :: (((state_callOps[c1] == mapDelete(u)) && (state_callOps[c2] == mapWrite(u, f, v))) ==> !(state_happensBefore[c1, c2])));
{
  var newInvocationId: invocationId;
  assume {:captureState "[line 64:0] start of procedure removeUser"} true;
  assume {:captureState "[line 64:27] "} true;
  assume {:captureState "[line 65:2] "} true;
  call beginAtomic();
  assume {:captureState "[line 65:2] begin atomic"} true;
  assume {:captureState "[line 65:2] "} true;
  call crdtOperation(mapDelete(id));
  assume {:captureState "[line 65:2] before commit"} true;
  call endAtomic();
  assume {:captureState "[line 65:20] end atomic"} true;
  assume {:captureState "[line 64:0] before return"} true;
  call newInvocationId := finishInvocation(invocation_removeUser(id));
  assume {:captureState "[line 66:1] end of procedure removeUser"} true;
}

procedure getUser(id: UserId) returns (result: getUserResult)
modifies state_knownIds_UserId, state_callOps, state_visibleCalls, state_happensBefore, state_sameTransaction, state_currentTransaction, state_maxId, state_origin, state_inCurrentInvocation, state_invocations, state_invocationHappensBefore;
requires WellFormed(state_knownIds_UserId, state_callOps, state_visibleCalls, state_happensBefore, state_sameTransaction, state_currentTransaction, state_maxId, state_origin, state_inCurrentInvocation, state_invocations, state_invocationHappensBefore);
requires (forall c: callId :: !(state_inCurrentInvocation[c]));
requires (forall r: invocationId, g: invocationId, u: UserId, res: getUserResult :: ((((state_invocations[r] == invocation_removeUser(u)) && (state_invocations[g] == invocation_getUser(u, res))) && state_invocationHappensBefore[r, g]) ==> (res == notFound())));
requires (forall u: UserId, i: invocationId :: ((state_invocations[i] == invocation_removeUser(u)) ==> (exists c: callId :: ((state_origin[c] == i) && (state_callOps[c] == mapDelete(u))))));
requires (forall c1: callId, c2: callId, u: UserId, f: userRecordField, v: String :: (((state_callOps[c1] == mapDelete(u)) && (state_callOps[c2] == mapWrite(u, f, v))) ==> !(state_happensBefore[c1, c2])));
ensures (forall r: invocationId, g: invocationId, u: UserId, res: getUserResult :: ((((state_invocations[r] == invocation_removeUser(u)) && (state_invocations[g] == invocation_getUser(u, res))) && state_invocationHappensBefore[r, g]) ==> (res == notFound())));
ensures (forall u: UserId, i: invocationId :: ((state_invocations[i] == invocation_removeUser(u)) ==> (exists c: callId :: ((state_origin[c] == i) && (state_callOps[c] == mapDelete(u))))));
ensures (forall c1: callId, c2: callId, u: UserId, f: userRecordField, v: String :: (((state_callOps[c1] == mapDelete(u)) && (state_callOps[c2] == mapWrite(u, f, v))) ==> !(state_happensBefore[c1, c2])));
{
  var __query_1: bool;
  var __query_2: String;
  var __query_3: String;
  var old_state_inCurrentInvocation: [callId]bool;
  var newInvocationId: invocationId;
  assume {:captureState "[line 68:0] start of procedure getUser"} true;
  assume {:captureState "[line 68:39] "} true;
  assume {:captureState "[line 69:2] "} true;
  call beginAtomic();
  assume {:captureState "[line 69:2] begin atomic"} true;
  assume {:captureState "[line 69:9] "} true;
  assume {:captureState "[line 70:8] "} true;
  __query_1 := mapExists(id, state_knownIds_UserId, state_callOps, state_visibleCalls, state_happensBefore, state_sameTransaction, state_currentTransaction, state_maxId, state_origin, state_inCurrentInvocation, state_invocations, state_invocationHappensBefore);
  assume {:captureState "[line 70:8] "} true;
  call crdtOperation(queryop_mapExists(id, __query_1));
  assume {:captureState "[line 70:4] "} true;
  if (__query_1) {
    assume {:captureState "[line 70:23] "} true;
    assume {:captureState "[line 71:19] "} true;
    __query_2 := mapGet(id, f_name(), state_knownIds_UserId, state_callOps, state_visibleCalls, state_happensBefore, state_sameTransaction, state_currentTransaction, state_maxId, state_origin, state_inCurrentInvocation, state_invocations, state_invocationHappensBefore);
    assume {:captureState "[line 71:19] "} true;
    call crdtOperation(queryop_mapGet(id, f_name(), __query_2));
    assume {:captureState "[line 71:41] "} true;
    __query_3 := mapGet(id, f_mail(), state_knownIds_UserId, state_callOps, state_visibleCalls, state_happensBefore, state_sameTransaction, state_currentTransaction, state_maxId, state_origin, state_inCurrentInvocation, state_invocations, state_invocationHappensBefore);
    assume {:captureState "[line 71:41] "} true;
    call crdtOperation(queryop_mapGet(id, f_mail(), __query_3));
    assume {:captureState "[line 71:6] "} true;
    call endAtomic();
    assume {:captureState "[line 71:6] before return"} true;


    old_state_inCurrentInvocation := state_inCurrentInvocation;

    call newInvocationId := finishInvocation(invocation_getUser(id, found(__query_2, __query_3)));

    assert (forall r: callId ::
        state_callOps[r] == mapDelete(id) ==> !state_visibleCalls[r]);

    assert (forall r: invocationId ::
            state_invocations[r] == invocation_removeUser(id)
              ==> (exists c: callId :: state_origin[c] == r && state_callOps[c] == mapDelete(id)));

    assert (forall r: invocationId ::
            state_invocations[r] == invocation_removeUser(id)
         && state_invocationHappensBefore[r, newInvocationId]
          ==> (exists c: callId ::
                      state_origin[c] == r
                   && state_callOps[c] == mapDelete(id)
                   && (forall c2: callId :: old_state_inCurrentInvocation[c2] ==> state_happensBefore[c, c2])));

    assert (forall r: invocationId ::
        !(state_invocations[r] == invocation_removeUser(id)
          && state_invocationHappensBefore[r, newInvocationId]));


    result := found(__query_2, __query_3); return;
  } else {
    assume {:captureState "[line 72:11] "} true;
    assume {:captureState "[line 73:6] "} true;
    call endAtomic();
    assume {:captureState "[line 73:6] before return"} true;
    call newInvocationId := finishInvocation(invocation_getUser(id, notFound()));
    result := notFound(); return;
  }
  assume {:captureState "[line 69:2] before commit"} true;
  call endAtomic();
  assume {:captureState "[line 75:3] end atomic"} true;
  assume {:captureState "[line 76:1] end of procedure getUser"} true;
}
