// package crdtver.language.crdts

// import crdtver.language.TypedAst
// import crdtver.language.TypedAst.{BoolType, TypeVarUse}
// import crdtver.language.TypedAstHelper._
// import crdtver.language.crdts.ACrdtInstance.{QueryStructure, printTypes}
// import crdtver.language.crdts.FlagCrdt.Strategy

// class FileAccessCrdt extends CrdtTypeDefinition {

//   /** number of normal type parameters */
//   override def numberTypes: Int = 2

//   /** number of CRDT type parameters */
//   override def numberInstances: Int = 0

//   private val FAOp = "FAOp"

//   private val Assign = "Assign"

//   private val FAQry = "FAQry"

//   private val ReadFA = "ReadFA"

//   // predefined values for access rights
//   private val None : TypedAst.InExpr = "None" // here is the problem, i cant define a InExpr type with single value for comparison in Strutegy

//   private val R : TypedAst.InExpr = "R" // here is the problem

//   private val W = "W"

//   private val X = "X"

//   private val RW = "RW"

//   private val RX = "RX"

//   private val WX = "WX"

//   private val RWX = "RWX"

//   // predefined values for user types 

//   private val Admin = "Admin"

//   private val User = "User"


//   override def additionalDataTypes: List[TypedAst.InTypeDecl] = List(
//     dataType(
//       FAOp,
//       List("U", "V"),
//       List(
//         dtCase(Assign, List("user" -> TypeVarUse("U")(), "value" -> TypeVarUse("V")()))
//       )
//     ),
//     dataType(FAQry, List("V"), List(dtCase(ReadFA, List())))
//   )

//   override def instantiate(typeArgs: List[TypedAst.InTypeExpr], crdtArgs: List[ACrdtInstance]): ACrdtInstance = new ACrdtInstance {
//     val U: TypedAst.InTypeExpr = typeArgs.head // it should be User
//     val V: TypedAst.InTypeExpr = (typeArgs.tail).head // make sure it will give the second item in list which must be Value

//     override def toString: String = s"${FileAccessCrdt.this.name}${printTypes(typeArgs, crdtArgs)}"

//     override def operationType: TypedAst.InTypeExpr = TypedAst.SimpleType(FAOp, List(U, V))()

//     override def queryType: TypedAst.InTypeExpr = TypedAst.SimpleType(FAQry, List(U, V))()

//     override def queryReturnType(q: QueryStructure): TypedAst.InTypeExpr = q match {
//       case QueryStructure(ReadFA, List()) => V
//     }

//     override def queryDefinitions(): List[TypedAst.InQueryDecl] = List(
//       queryDeclEnsures(ReadFA, List(), V, {
//         val result = varUse("result", V)
//         val user1 = varUse("user1", U)
//         val c = varUse("c")
//         val c2 = varUse("c2")
//         val v = varUse("v", V)
//         val user2 = varUse("user2", U)
//         not(exists(v, v !== None || v !== R || v !== W || v !== X || v !== RW || v !== RX || v !== WX || v !== RWX  ))
//         &&
//         not(exists(result, result !== None || result !== R || result !== W || result !== X || result !== RW || result !== RX || result !== WX || result !== RWX  ))
//         &&
//         not(exists(user1, user1 !== Admin || user1 !== User))
//         &&
//         not(exists(user2, user2 !== Admin || user2 !== User))
//         &&
        
//         ( 
//           not(exists(c, exists(v, c.isVis && c.op === makeOp(Assign, user2, v) && user2 === Admin))) ||
//           (
//             exists(c, c.isVis && c.op === makeOp(Assign, user1, result) && user1 === Admin && result === R 
//                 && not(exists(c2, exists(v, c2.isVis && c2.op === makeOp(Assign, user2, v) && user2 === Admin && c < c2 && v === None
//                      )))
//                 )
//             &&
//             exists(c, c.isVis && c.op === makeOp(Assign, user1, result) && user1 === Admin && result === W 
//                     && not(exists(c2, exists(v, c2.isVis && c2.op === makeOp(Assign, user2, v) && user2 === Admin && c < c2 && v === None //(v === None || v === R)
//                         )))
//                     )
//             &&
//             exists(c, c.isVis && c.op === makeOp(Assign, user1, result) && user1 === Admin && result === E
//                     && not(exists(c2, exists(v, c2.isVis && c2.op === makeOp(Assign, user2, v) && user2 === Admin && c < c2 && v === None //(v === None || v === R)
//                         )))
//                     )
//             &&
//             exists(c, c.isVis && c.op === makeOp(Assign, user1, result) && user1 === Admin && result === RW
//                     && not(exists(c2, exists(v, c2.isVis && c2.op === makeOp(Assign, user2, v) && user2 === Admin && c < c2 && (v === None || v === R || v === W )
//                         )))
//                     )
//             &&
//             exists(c, c.isVis && c.op === makeOp(Assign, user1, result) && user1 === Admin && result === RX
//                     && not(exists(c2, exists(v, c2.isVis && c2.op === makeOp(Assign, user2, v) && user2 === Admin && c < c2 && (v === None || v === X || v === R )
//                         )))
//                     )
//             &&
//             exists(c, c.isVis && c.op === makeOp(Assign, user1, result) && user1 === Admin && result === WX
//                     && not(exists(c2, exists(v, c2.isVis && c2.op === makeOp(Assign, user2, v) && user2 === Admin && c < c2 && (v === None || v === X || v === W )
//                         )))
//                     )
//             &&
//             exists(c, c.isVis && c.op === makeOp(Assign, user1, result) && user1 === Admin && result === RWX
//                     && not(exists(c2, exists(v, c2.isVis && c2.op === makeOp(Assign, user2, v) && user2 === Admin && c < c2 && (v === None || v === R || v === W || X)
//                         )))
//                     )
//           )
//         )

//         &&
//         (
//           exists(c, c.isVis && c.op === makeOp(Assign, user1, result) && user1 === User
//                     && not(exists(c2, exists(v, c2.isVis && c2.op === makeOp(Assign, user2, v) && user2 === Admin
//                         )))
//                     )
//         )
      
//       })

//     )

//     override def additionalDataTypesRec: List[TypedAst.InTypeDecl] = FileAccessCrdt.this.additionalDataTypes
//   }

//   /** name of the CRDT */
//   override def name: String = "FileAccessTuple"
// }
