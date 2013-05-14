.class public genFile
.super java/lang/Object

.method public <init>()V
aload_0
invokespecial java/lang/Object/<init>()V
return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 1
invokestatic genFile/main()V
pop
return
.end method

.method public static main()I
.limit locals 0
.limit stack 0
entry:
iload 0
invokestatic Runtime/printInt(I)V
dload 1
invokestatic Runtime/printDouble(D)V
ldc 0
ireturn
.end method

