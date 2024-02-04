
InsertionTailCallOpt.exe:     file format elf64-x86-64


Disassembly of section .init:

0000000000401000 <_init>:
_init():
  401000:	f3 0f 1e fa          	endbr64
  401004:	48 83 ec 08          	sub    $0x8,%rsp
  401008:	48 8b 05 d1 3f 00 00 	mov    0x3fd1(%rip),%rax        # 404fe0 <__gmon_start__@Base>
  40100f:	48 85 c0             	test   %rax,%rax
  401012:	74 02                	je     401016 <_init+0x16>
  401014:	ff d0                	call   *%rax
  401016:	48 83 c4 08          	add    $0x8,%rsp
  40101a:	c3                   	ret

Disassembly of section .plt:

0000000000401020 <free@plt-0x10>:
  401020:	ff 35 ca 3f 00 00    	push   0x3fca(%rip)        # 404ff0 <_GLOBAL_OFFSET_TABLE_+0x8>
  401026:	ff 25 cc 3f 00 00    	jmp    *0x3fcc(%rip)        # 404ff8 <_GLOBAL_OFFSET_TABLE_+0x10>
  40102c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401030 <free@plt>:
  401030:	ff 25 ca 3f 00 00    	jmp    *0x3fca(%rip)        # 405000 <free@GLIBC_2.2.5>
  401036:	68 00 00 00 00       	push   $0x0
  40103b:	e9 e0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401040 <putchar@plt>:
  401040:	ff 25 c2 3f 00 00    	jmp    *0x3fc2(%rip)        # 405008 <putchar@GLIBC_2.2.5>
  401046:	68 01 00 00 00       	push   $0x1
  40104b:	e9 d0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401050 <strncpy@plt>:
  401050:	ff 25 ba 3f 00 00    	jmp    *0x3fba(%rip)        # 405010 <strncpy@GLIBC_2.2.5>
  401056:	68 02 00 00 00       	push   $0x2
  40105b:	e9 c0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401060 <strcpy@plt>:
  401060:	ff 25 b2 3f 00 00    	jmp    *0x3fb2(%rip)        # 405018 <strcpy@GLIBC_2.2.5>
  401066:	68 03 00 00 00       	push   $0x3
  40106b:	e9 b0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401070 <gib_init_zcts@plt>:
  401070:	ff 25 aa 3f 00 00    	jmp    *0x3faa(%rip)        # 405020 <gib_init_zcts@Base>
  401076:	68 04 00 00 00       	push   $0x4
  40107b:	e9 a0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401080 <puts@plt>:
  401080:	ff 25 a2 3f 00 00    	jmp    *0x3fa2(%rip)        # 405028 <puts@GLIBC_2.2.5>
  401086:	68 05 00 00 00       	push   $0x5
  40108b:	e9 90 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401090 <qsort@plt>:
  401090:	ff 25 9a 3f 00 00    	jmp    *0x3f9a(%rip)        # 405030 <qsort@GLIBC_2.2.5>
  401096:	68 06 00 00 00       	push   $0x6
  40109b:	e9 80 ff ff ff       	jmp    401020 <_init+0x20>

00000000004010a0 <clock_gettime@plt>:
  4010a0:	ff 25 92 3f 00 00    	jmp    *0x3f92(%rip)        # 405038 <clock_gettime@GLIBC_2.17>
  4010a6:	68 07 00 00 00       	push   $0x7
  4010ab:	e9 70 ff ff ff       	jmp    401020 <_init+0x20>

00000000004010b0 <strlen@plt>:
  4010b0:	ff 25 8a 3f 00 00    	jmp    *0x3f8a(%rip)        # 405040 <strlen@GLIBC_2.2.5>
  4010b6:	68 08 00 00 00       	push   $0x8
  4010bb:	e9 60 ff ff ff       	jmp    401020 <_init+0x20>

00000000004010c0 <printf@plt>:
  4010c0:	ff 25 82 3f 00 00    	jmp    *0x3f82(%rip)        # 405048 <printf@GLIBC_2.2.5>
  4010c6:	68 09 00 00 00       	push   $0x9
  4010cb:	e9 50 ff ff ff       	jmp    401020 <_init+0x20>

00000000004010d0 <gib_gc_cleanup@plt>:
  4010d0:	ff 25 7a 3f 00 00    	jmp    *0x3f7a(%rip)        # 405050 <gib_gc_cleanup@Base>
  4010d6:	68 0a 00 00 00       	push   $0xa
  4010db:	e9 40 ff ff ff       	jmp    401020 <_init+0x20>

00000000004010e0 <__assert_fail@plt>:
  4010e0:	ff 25 72 3f 00 00    	jmp    *0x3f72(%rip)        # 405058 <__assert_fail@GLIBC_2.2.5>
  4010e6:	68 0b 00 00 00       	push   $0xb
  4010eb:	e9 30 ff ff ff       	jmp    401020 <_init+0x20>

00000000004010f0 <gib_info_table_initialize@plt>:
  4010f0:	ff 25 6a 3f 00 00    	jmp    *0x3f6a(%rip)        # 405060 <gib_info_table_initialize@Base>
  4010f6:	68 0c 00 00 00       	push   $0xc
  4010fb:	e9 20 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401100 <calloc@plt>:
  401100:	ff 25 62 3f 00 00    	jmp    *0x3f62(%rip)        # 405068 <calloc@GLIBC_2.2.5>
  401106:	68 0d 00 00 00       	push   $0xd
  40110b:	e9 10 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401110 <strcmp@plt>:
  401110:	ff 25 5a 3f 00 00    	jmp    *0x3f5a(%rip)        # 405070 <strcmp@GLIBC_2.2.5>
  401116:	68 0e 00 00 00       	push   $0xe
  40111b:	e9 00 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401120 <strtoll@plt>:
  401120:	ff 25 52 3f 00 00    	jmp    *0x3f52(%rip)        # 405078 <strtoll@GLIBC_2.2.5>
  401126:	68 0f 00 00 00       	push   $0xf
  40112b:	e9 f0 fe ff ff       	jmp    401020 <_init+0x20>

0000000000401130 <fprintf@plt>:
  401130:	ff 25 4a 3f 00 00    	jmp    *0x3f4a(%rip)        # 405080 <fprintf@GLIBC_2.2.5>
  401136:	68 10 00 00 00       	push   $0x10
  40113b:	e9 e0 fe ff ff       	jmp    401020 <_init+0x20>

0000000000401140 <memcpy@plt>:
  401140:	ff 25 42 3f 00 00    	jmp    *0x3f42(%rip)        # 405088 <memcpy@GLIBC_2.14>
  401146:	68 11 00 00 00       	push   $0x11
  40114b:	e9 d0 fe ff ff       	jmp    401020 <_init+0x20>

0000000000401150 <gib_print_rust_gc_config@plt>:
  401150:	ff 25 3a 3f 00 00    	jmp    *0x3f3a(%rip)        # 405090 <gib_print_rust_gc_config@Base>
  401156:	68 12 00 00 00       	push   $0x12
  40115b:	e9 c0 fe ff ff       	jmp    401020 <_init+0x20>

0000000000401160 <malloc@plt>:
  401160:	ff 25 32 3f 00 00    	jmp    *0x3f32(%rip)        # 405098 <malloc@GLIBC_2.2.5>
  401166:	68 13 00 00 00       	push   $0x13
  40116b:	e9 b0 fe ff ff       	jmp    401020 <_init+0x20>

0000000000401170 <fflush@plt>:
  401170:	ff 25 2a 3f 00 00    	jmp    *0x3f2a(%rip)        # 4050a0 <fflush@GLIBC_2.2.5>
  401176:	68 14 00 00 00       	push   $0x14
  40117b:	e9 a0 fe ff ff       	jmp    401020 <_init+0x20>

0000000000401180 <setrlimit@plt>:
  401180:	ff 25 22 3f 00 00    	jmp    *0x3f22(%rip)        # 4050a8 <setrlimit@GLIBC_2.2.5>
  401186:	68 15 00 00 00       	push   $0x15
  40118b:	e9 90 fe ff ff       	jmp    401020 <_init+0x20>

0000000000401190 <gib_get_rust_struct_sizes@plt>:
  401190:	ff 25 1a 3f 00 00    	jmp    *0x3f1a(%rip)        # 4050b0 <gib_get_rust_struct_sizes@Base>
  401196:	68 16 00 00 00       	push   $0x16
  40119b:	e9 80 fe ff ff       	jmp    401020 <_init+0x20>

00000000004011a0 <gib_info_table_finalize@plt>:
  4011a0:	ff 25 12 3f 00 00    	jmp    *0x3f12(%rip)        # 4050b8 <gib_info_table_finalize@Base>
  4011a6:	68 17 00 00 00       	push   $0x17
  4011ab:	e9 70 fe ff ff       	jmp    401020 <_init+0x20>

00000000004011b0 <exit@plt>:
  4011b0:	ff 25 0a 3f 00 00    	jmp    *0x3f0a(%rip)        # 4050c0 <exit@GLIBC_2.2.5>
  4011b6:	68 18 00 00 00       	push   $0x18
  4011bb:	e9 60 fe ff ff       	jmp    401020 <_init+0x20>

00000000004011c0 <fwrite@plt>:
  4011c0:	ff 25 02 3f 00 00    	jmp    *0x3f02(%rip)        # 4050c8 <fwrite@GLIBC_2.2.5>
  4011c6:	68 19 00 00 00       	push   $0x19
  4011cb:	e9 50 fe ff ff       	jmp    401020 <_init+0x20>

00000000004011d0 <getrlimit@plt>:
  4011d0:	ff 25 fa 3e 00 00    	jmp    *0x3efa(%rip)        # 4050d0 <getrlimit@GLIBC_2.2.5>
  4011d6:	68 1a 00 00 00       	push   $0x1a
  4011db:	e9 40 fe ff ff       	jmp    401020 <_init+0x20>

Disassembly of section .text:

00000000004011e0 <main>:
main():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:405
            return tailapp_1228;
        }
    }
}
int main(int argc, char **argv)
{
  4011e0:	41 57                	push   %r15
  4011e2:	41 56                	push   %r14
  4011e4:	41 55                	push   %r13
  4011e6:	41 54                	push   %r12
  4011e8:	55                   	push   %rbp
  4011e9:	53                   	push   %rbx
  4011ea:	89 fb                	mov    %edi,%ebx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1029
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */


void gib_print_gc_config(void) {
    printf("Rust config\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
  4011ec:	bf f0 34 40 00       	mov    $0x4034f0,%edi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:405
  4011f1:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  4011f8:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1029
  4011fd:	e8 7e fe ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1030
    fflush(stdout);
  401202:	48 8b 3d 17 3f 00 00 	mov    0x3f17(%rip),%rdi        # 405120 <stdout@GLIBC_2.2.5>
  401209:	e8 62 ff ff ff       	call   401170 <fflush@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1031
    gib_print_rust_gc_config();
  40120e:	e8 3d ff ff ff       	call   401150 <gib_print_rust_gc_config@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1032
    fflush(stdout);
  401213:	48 8b 3d 06 3f 00 00 	mov    0x3f06(%rip),%rdi        # 405120 <stdout@GLIBC_2.2.5>
  40121a:	e8 51 ff ff ff       	call   401170 <fflush@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1033
    printf("\n");
  40121f:	bf 0a 00 00 00       	mov    $0xa,%edi
  401224:	e8 17 fe ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1036


    printf("C config\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
  401229:	bf 28 35 40 00       	mov    $0x403528,%edi
  40122e:	e8 4d fe ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1040

#if defined _GIBBON_GENGC && _GIBBON_GENGC == 0
    #pragma message "Generational GC is disabled."
    printf("Generational GC is disabled.\n");
  401233:	bf 43 37 40 00       	mov    $0x403743,%edi
  401238:	e8 43 fe ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1051
#if defined _GIBBON_EAGER_PROMOTION && _GIBBON_EAGER_PROMOTION == 0
    #pragma message "Eager promotion is disabled."
    printf("Eager promotion is disabled.\n");
#else
    #pragma message "Eager promotion is enabled."
    printf("Eager promotion is enabled.\n");
  40123d:	bf 60 37 40 00       	mov    $0x403760,%edi
  401242:	e8 39 fe ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1056
#endif

#if defined _GIBBON_SIMPLE_WRITE_BARRIER && _GIBBON_SIMPLE_WRITE_BARRIER == 0
    #pragma message "Simple write barrier is disabled."
    printf("Simple write barrier is disabled.\n");
  401247:	bf 60 35 40 00       	mov    $0x403560,%edi
  40124c:	e8 2f fe ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1062
#else
    #pragma message "Simple write barrier is enabled."
    printf("Simple write barrier is enabled.\n");
#endif

    printf("Nursery size=%zu\n", (size_t) gib_nursery_size);
  401251:	be 00 00 40 00       	mov    $0x400000,%esi
  401256:	bf 7c 37 40 00       	mov    $0x40377c,%edi
  40125b:	31 c0                	xor    %eax,%eax
  40125d:	e8 5e fe ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1063
    printf("Max chunk size=%zu\n", (size_t) GIB_MAX_CHUNK_SIZE);
  401262:	be dc ff 00 00       	mov    $0xffdc,%esi
  401267:	bf 8e 37 40 00       	mov    $0x40378e,%edi
  40126c:	31 c0                	xor    %eax,%eax
  40126e:	e8 4d fe ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1064
    printf("Initial chunk size=%zu\n", (size_t) GIB_INIT_CHUNK_SIZE);
  401273:	be 00 02 00 00       	mov    $0x200,%esi
  401278:	bf a2 37 40 00       	mov    $0x4037a2,%edi
  40127d:	31 c0                	xor    %eax,%eax
  40127f:	e8 3c fe ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1065
    printf("\n");
  401284:	bf 0a 00 00 00       	mov    $0xa,%edi
  401289:	e8 b2 fd ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1900
    // Print the GC configuration.
    gib_print_gc_config();
#endif

    // Ensure that C and Rust agree on sizes of structs that cross the boundary.
    gib_check_rust_struct_sizes();
  40128e:	e8 9d 1a 00 00       	call   402d30 <gib_check_rust_struct_sizes>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1909
    //   num iterations: How many times to repeat a benchmark.
    //   tree size: An integer passes to `build_tree()`.

    struct rlimit lim;
    int code;
    if ( (code = getrlimit(RLIMIT_STACK, &lim)) ) {
  401293:	48 8d 74 24 70       	lea    0x70(%rsp),%rsi
  401298:	bf 03 00 00 00       	mov    $0x3,%edi
  40129d:	e8 2e ff ff ff       	call   4011d0 <getrlimit@plt>
  4012a2:	89 44 24 10          	mov    %eax,0x10(%rsp)
  4012a6:	85 c0                	test   %eax,%eax
  4012a8:	0f 85 df 0c 00 00    	jne    401f8d <main+0xdad>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1914
        fprintf(stderr, " [gibbon rts] failed to getrlimit, code %d\n", code);
        exit(1);
    }

    lim.rlim_cur = 4 * 1024LU * 1024LU * 1024LU; // 1GB stack.
  4012ae:	48 b8 00 00 00 00 01 	movabs $0x100000000,%rax
  4012b5:	00 00 00 
  4012b8:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  4012bd:	eb 32                	jmp    4012f1 <main+0x111>
  4012bf:	90                   	nop
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1922

    // WARNING: Haven't yet figured out why this doesn't work on MacOS...
#ifndef __APPLE__
    code = setrlimit(RLIMIT_STACK, &lim);
    while (code) {
        fprintf(stderr, " [gibbon rts] Failed to set stack size to %lu, code %d\n",
  4012c0:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  4012c5:	48 8b 3d 74 3e 00 00 	mov    0x3e74(%rip),%rdi        # 405140 <stderr@GLIBC_2.2.5>
  4012cc:	be b8 35 40 00       	mov    $0x4035b8,%esi
  4012d1:	31 c0                	xor    %eax,%eax
  4012d3:	e8 58 fe ff ff       	call   401130 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1924
                (uint64_t)lim.rlim_cur, code);
        lim.rlim_cur /= 2;
  4012d8:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  4012dd:	48 d1 e8             	shr    $1,%rax
  4012e0:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1926
        // lim.rlim_max /= 2;
        if(lim.rlim_cur < 100 * 1024) {
  4012e5:	48 3d ff 8f 01 00    	cmp    $0x18fff,%rax
  4012eb:	0f 86 ca 02 00 00    	jbe    4015bb <main+0x3db>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1920
    code = setrlimit(RLIMIT_STACK, &lim);
  4012f1:	48 8d 74 24 70       	lea    0x70(%rsp),%rsi
  4012f6:	bf 03 00 00 00       	mov    $0x3,%edi
  4012fb:	e8 80 fe ff ff       	call   401180 <setrlimit@plt>
  401300:	89 c1                	mov    %eax,%ecx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1921
    while (code) {
  401302:	85 c0                	test   %eax,%eax
  401304:	75 ba                	jne    4012c0 <main+0xe0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1937
#endif

    // int got_numargs = argc; // How many numeric arguments have we got.

    int i;
    for (i = 1; i < argc; ++i)
  401306:	83 fb 01             	cmp    $0x1,%ebx
  401309:	0f 8e cc 02 00 00    	jle    4015db <main+0x3fb>
  40130f:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  401314:	8d 6b fe             	lea    -0x2(%rbx),%ebp
  401317:	41 be 02 00 00 00    	mov    $0x2,%r14d
  40131d:	41 bf 01 00 00 00    	mov    $0x1,%r15d
  401323:	83 e5 fe             	and    $0xfffffffe,%ebp
  401326:	4c 8d 68 10          	lea    0x10(%rax),%r13
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1968
        else if (strcmp(argv[i], "--array-input-length") == 0 && i < argc - 1) {
            check_args(i, argc, argv, "--array-input-length");
            gib_global_arrayfile_length_param = atoll(argv[i+1]);
            i++;
        }
        else if (strcmp(argv[i], "--bench-prog") == 0 && i < argc - 1) {
  40132a:	8d 43 ff             	lea    -0x1(%rbx),%eax
  40132d:	83 c5 03             	add    $0x3,%ebp
  401330:	89 44 24 08          	mov    %eax,0x8(%rsp)
  401334:	eb 4a                	jmp    401380 <main+0x1a0>
  401336:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40133d:	00 00 00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1943
        else if (strcmp(argv[i], "--biginf-buffer-size") == 0 && i < argc - 1) {
  401340:	44 39 7c 24 08       	cmp    %r15d,0x8(%rsp)
  401345:	0f 8e d5 00 00 00    	jle    401420 <main+0x240>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  40134b:	44 39 f3             	cmp    %r14d,%ebx
  40134e:	0f 8e be 0b 00 00    	jle    401f12 <main+0xd32>
/usr/include/stdlib.h:376

# ifdef __USE_ISOC99
__extension__ __extern_inline long long int
__NTH (atoll (const char *__nptr))
{
  return strtoll (__nptr, (char **) NULL, 10);
  401354:	49 8b 7d 00          	mov    0x0(%r13),%rdi
  401358:	ba 0a 00 00 00       	mov    $0xa,%edx
  40135d:	31 f6                	xor    %esi,%esi
  40135f:	e8 bc fd ff ff       	call   401120 <strtoll@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1945
            gib_global_biginf_init_chunk_size = atoll(argv[i + 1]);
  401364:	48 89 05 85 3d 00 00 	mov    %rax,0x3d85(%rip)        # 4050f0 <gib_global_biginf_init_chunk_size>
  40136b:	41 83 c7 02          	add    $0x2,%r15d
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1937
    for (i = 1; i < argc; ++i)
  40136f:	41 83 c6 02          	add    $0x2,%r14d
  401373:	49 83 c5 10          	add    $0x10,%r13
  401377:	41 39 ef             	cmp    %ebp,%r15d
  40137a:	0f 84 5b 02 00 00    	je     4015db <main+0x3fb>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1939
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
  401380:	4d 8b 45 f8          	mov    -0x8(%r13),%r8
  401384:	41 80 38 2d          	cmpb   $0x2d,(%r8)
  401388:	75 12                	jne    40139c <main+0x1bc>
  40138a:	41 80 78 01 68       	cmpb   $0x68,0x1(%r8)
  40138f:	75 0b                	jne    40139c <main+0x1bc>
  401391:	41 80 78 02 00       	cmpb   $0x0,0x2(%r8)
  401396:	0f 84 89 01 00 00    	je     401525 <main+0x345>
  40139c:	bf ba 37 40 00       	mov    $0x4037ba,%edi
  4013a1:	b9 07 00 00 00       	mov    $0x7,%ecx
  4013a6:	4c 89 c6             	mov    %r8,%rsi
  4013a9:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  4013ab:	0f 97 c0             	seta   %al
  4013ae:	1c 00                	sbb    $0x0,%al
  4013b0:	84 c0                	test   %al,%al
  4013b2:	0f 84 6d 01 00 00    	je     401525 <main+0x345>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1943
        else if (strcmp(argv[i], "--biginf-buffer-size") == 0 && i < argc - 1) {
  4013b8:	bf c1 37 40 00       	mov    $0x4037c1,%edi
  4013bd:	b9 15 00 00 00       	mov    $0x15,%ecx
  4013c2:	4c 89 c6             	mov    %r8,%rsi
  4013c5:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  4013c7:	0f 97 c0             	seta   %al
  4013ca:	1c 00                	sbb    $0x0,%al
  4013cc:	84 c0                	test   %al,%al
  4013ce:	0f 84 6c ff ff ff    	je     401340 <main+0x160>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1948
        else if (strcmp(argv[i], "--inf-buffer-size") == 0 && i < argc - 1) {
  4013d4:	bf d6 37 40 00       	mov    $0x4037d6,%edi
  4013d9:	b9 12 00 00 00       	mov    $0x12,%ecx
  4013de:	4c 89 c6             	mov    %r8,%rsi
  4013e1:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  4013e3:	0f 97 c0             	seta   %al
  4013e6:	1c 00                	sbb    $0x0,%al
  4013e8:	84 c0                	test   %al,%al
  4013ea:	75 34                	jne    401420 <main+0x240>
  4013ec:	44 39 7c 24 08       	cmp    %r15d,0x8(%rsp)
  4013f1:	7e 2d                	jle    401420 <main+0x240>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  4013f3:	44 39 f3             	cmp    %r14d,%ebx
  4013f6:	0f 8e 82 0b 00 00    	jle    401f7e <main+0xd9e>
/usr/include/stdlib.h:376
  4013fc:	49 8b 7d 00          	mov    0x0(%r13),%rdi
  401400:	ba 0a 00 00 00       	mov    $0xa,%edx
  401405:	31 f6                	xor    %esi,%esi
  401407:	e8 14 fd ff ff       	call   401120 <strtoll@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1950
            gib_global_inf_init_chunk_size = atoll(argv[i + 1]);
  40140c:	48 89 05 d5 3c 00 00 	mov    %rax,0x3cd5(%rip)        # 4050e8 <gib_global_inf_init_chunk_size>
  401413:	e9 53 ff ff ff       	jmp    40136b <main+0x18b>
  401418:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40141f:	00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1953
        else if ((strcmp(argv[i], "--bench-input") == 0)) {
  401420:	bf e8 37 40 00       	mov    $0x4037e8,%edi
  401425:	b9 0e 00 00 00       	mov    $0xe,%ecx
  40142a:	4c 89 c6             	mov    %r8,%rsi
  40142d:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  40142f:	0f 97 c0             	seta   %al
  401432:	1c 00                	sbb    $0x0,%al
  401434:	84 c0                	test   %al,%al
  401436:	75 18                	jne    401450 <main+0x270>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  401438:	44 39 f3             	cmp    %r14d,%ebx
  40143b:	0f 8f 2a ff ff ff    	jg     40136b <main+0x18b>
  401441:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  401446:	be e8 37 40 00       	mov    $0x4037e8,%esi
  40144b:	e8 10 15 00 00       	call   402960 <check_args.part.0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1958
        else if ((strcmp(argv[i], "--array-input") == 0)) {
  401450:	bf f6 37 40 00       	mov    $0x4037f6,%edi
  401455:	b9 0e 00 00 00       	mov    $0xe,%ecx
  40145a:	4c 89 c6             	mov    %r8,%rsi
  40145d:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  40145f:	0f 97 c0             	seta   %al
  401462:	1c 00                	sbb    $0x0,%al
  401464:	84 c0                	test   %al,%al
  401466:	75 18                	jne    401480 <main+0x2a0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  401468:	44 39 f3             	cmp    %r14d,%ebx
  40146b:	0f 8f fa fe ff ff    	jg     40136b <main+0x18b>
  401471:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  401476:	be f6 37 40 00       	mov    $0x4037f6,%esi
  40147b:	e8 e0 14 00 00       	call   402960 <check_args.part.0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1963
        else if (strcmp(argv[i], "--array-input-length") == 0 && i < argc - 1) {
  401480:	bf 04 38 40 00       	mov    $0x403804,%edi
  401485:	b9 15 00 00 00       	mov    $0x15,%ecx
  40148a:	4c 89 c6             	mov    %r8,%rsi
  40148d:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  40148f:	0f 97 c0             	seta   %al
  401492:	1c 00                	sbb    $0x0,%al
  401494:	84 c0                	test   %al,%al
  401496:	75 28                	jne    4014c0 <main+0x2e0>
  401498:	44 39 7c 24 08       	cmp    %r15d,0x8(%rsp)
  40149d:	7e 49                	jle    4014e8 <main+0x308>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  40149f:	44 39 f3             	cmp    %r14d,%ebx
  4014a2:	0f 8e 0d 0b 00 00    	jle    401fb5 <main+0xdd5>
/usr/include/stdlib.h:376
  4014a8:	49 8b 7d 00          	mov    0x0(%r13),%rdi
  4014ac:	ba 0a 00 00 00       	mov    $0xa,%edx
  4014b1:	31 f6                	xor    %esi,%esi
  4014b3:	e8 68 fc ff ff       	call   401120 <strtoll@plt>
  4014b8:	e9 ae fe ff ff       	jmp    40136b <main+0x18b>
  4014bd:	0f 1f 00             	nopl   (%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1968
        else if (strcmp(argv[i], "--bench-prog") == 0 && i < argc - 1) {
  4014c0:	bf 19 38 40 00       	mov    $0x403819,%edi
  4014c5:	b9 0d 00 00 00       	mov    $0xd,%ecx
  4014ca:	4c 89 c6             	mov    %r8,%rsi
  4014cd:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  4014cf:	0f 97 c0             	seta   %al
  4014d2:	1c 00                	sbb    $0x0,%al
  4014d4:	84 c0                	test   %al,%al
  4014d6:	75 10                	jne    4014e8 <main+0x308>
  4014d8:	44 39 7c 24 08       	cmp    %r15d,0x8(%rsp)
  4014dd:	0f 8f 8d 00 00 00    	jg     401570 <main+0x390>
  4014e3:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1975
            int len = strlen(argv[i+1]);
            gib_global_bench_prog_param = (char*) gib_alloc((len+1)*sizeof(char));
            strncpy(gib_global_bench_prog_param,argv[i+1],len);
            i++;
        }
        else if ((strcmp(argv[i], "--iterate") == 0)) {
  4014e8:	bf 26 38 40 00       	mov    $0x403826,%edi
  4014ed:	b9 0a 00 00 00       	mov    $0xa,%ecx
  4014f2:	4c 89 c6             	mov    %r8,%rsi
  4014f5:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  4014f7:	0f 97 c0             	seta   %al
  4014fa:	1c 00                	sbb    $0x0,%al
  4014fc:	84 c0                	test   %al,%al
  4014fe:	75 36                	jne    401536 <main+0x356>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  401500:	44 39 f3             	cmp    %r14d,%ebx
  401503:	0f 8e 66 0a 00 00    	jle    401f6f <main+0xd8f>
/usr/include/stdlib.h:376
  401509:	49 8b 7d 00          	mov    0x0(%r13),%rdi
  40150d:	ba 0a 00 00 00       	mov    $0xa,%edx
  401512:	31 f6                	xor    %esi,%esi
  401514:	e8 07 fc ff ff       	call   401120 <strtoll@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1977
            check_args(i, argc, argv, "--iterate");
            gib_global_iters_param = atoll(argv[i+1]);
  401519:	48 89 05 d8 3b 00 00 	mov    %rax,0x3bd8(%rip)        # 4050f8 <gib_global_iters_param>
  401520:	e9 46 fe ff ff       	jmp    40136b <main+0x18b>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1940
            gib_show_usage(argv);
  401525:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40152a:	e8 71 13 00 00       	call   4028a0 <gib_show_usage>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1941
            exit(0);
  40152f:	31 ff                	xor    %edi,%edi
  401531:	e8 7a fc ff ff       	call   4011b0 <exit@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1980
            i++;
        }
        else if ((strcmp(argv[i], "--size-param") == 0)) {
  401536:	be 30 38 40 00       	mov    $0x403830,%esi
  40153b:	4c 89 c7             	mov    %r8,%rdi
  40153e:	e8 cd fb ff ff       	call   401110 <strcmp@plt>
  401543:	85 c0                	test   %eax,%eax
  401545:	0f 85 88 0a 00 00    	jne    401fd3 <main+0xdf3>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  40154b:	44 39 f3             	cmp    %r14d,%ebx
  40154e:	0f 8e 70 0a 00 00    	jle    401fc4 <main+0xde4>
/usr/include/stdlib.h:376
  401554:	49 8b 7d 00          	mov    0x0(%r13),%rdi
  401558:	ba 0a 00 00 00       	mov    $0xa,%edx
  40155d:	31 f6                	xor    %esi,%esi
  40155f:	e8 bc fb ff ff       	call   401120 <strtoll@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1982
            check_args(i, argc, argv, "--size-param");
            gib_global_size_param = atoll(argv[i+1]);
  401564:	48 89 05 95 3b 00 00 	mov    %rax,0x3b95(%rip)        # 405100 <gib_global_size_param>
  40156b:	e9 fb fd ff ff       	jmp    40136b <main+0x18b>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  401570:	44 39 f3             	cmp    %r14d,%ebx
  401573:	0f 8e c7 0a 00 00    	jle    402040 <main+0xe60>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1970
            int len = strlen(argv[i+1]);
  401579:	49 8b 75 00          	mov    0x0(%r13),%rsi
  40157d:	48 89 f7             	mov    %rsi,%rdi
  401580:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  401585:	e8 26 fb ff ff       	call   4010b0 <strlen@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1971
            gib_global_bench_prog_param = (char*) gib_alloc((len+1)*sizeof(char));
  40158a:	8d 78 01             	lea    0x1(%rax),%edi
  40158d:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401592:	48 63 ff             	movslq %edi,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401595:	e8 c6 fb ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1972
            strncpy(gib_global_bench_prog_param,argv[i+1],len);
  40159a:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40159f:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4015a4:	48 89 c7             	mov    %rax,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1971
            gib_global_bench_prog_param = (char*) gib_alloc((len+1)*sizeof(char));
  4015a7:	48 89 05 da 3b 00 00 	mov    %rax,0x3bda(%rip)        # 405188 <gib_global_bench_prog_param>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1972
            strncpy(gib_global_bench_prog_param,argv[i+1],len);
  4015ae:	48 63 d2             	movslq %edx,%rdx
  4015b1:	e8 9a fa ff ff       	call   401050 <strncpy@plt>
  4015b6:	e9 b0 fd ff ff       	jmp    40136b <main+0x18b>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1927
            fprintf(stderr, " [gibbon rts] Failed setrlimit stack size to something reasonable; giving up.\n");
  4015bb:	ba 4e 00 00 00       	mov    $0x4e,%edx
  4015c0:	be 01 00 00 00       	mov    $0x1,%esi
  4015c5:	bf f0 35 40 00       	mov    $0x4035f0,%edi
  4015ca:	48 8b 0d 6f 3b 00 00 	mov    0x3b6f(%rip),%rcx        # 405140 <stderr@GLIBC_2.2.5>
  4015d1:	e8 ea fb ff ff       	call   4011c0 <fwrite@plt>
  4015d6:	e9 2b fd ff ff       	jmp    401306 <main+0x126>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1996
        }
    }

    // Initialize gib_global_bench_prog_param to an empty string in case
    // the runtime argument --bench-prog isn't passed.
    if (gib_global_bench_prog_param == NULL) {
  4015db:	48 83 3d a5 3b 00 00 	cmpq   $0x0,0x3ba5(%rip)        # 405188 <gib_global_bench_prog_param>
  4015e2:	00 
  4015e3:	0f 84 e1 08 00 00    	je     401eca <main+0xcea>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:2007
#ifdef _GIBBON_PARALLEL
    gib_global_num_threads = __cilkrts_get_nworkers();
#endif

#if defined _GIBBON_VERBOSITY && _GIBBON_VERBOSITY >= 2
    printf("Number of threads: %ld\n", gib_global_num_threads);
  4015e9:	be 01 00 00 00       	mov    $0x1,%esi
  4015ee:	bf 5d 38 40 00       	mov    $0x40385d,%edi
  4015f3:	31 c0                	xor    %eax,%eax
  4015f5:	e8 c6 fa ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4015fa:	bf f0 00 00 00       	mov    $0xf0,%edi
  4015ff:	e8 5c fb ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1565
    stats->minor_collections = 0;
  401604:	66 0f ef c0          	pxor   %xmm0,%xmm0
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1581
    stats->gc_elapsed_time = 0;
  401608:	66 0f ef c9          	pxor   %xmm1,%xmm1
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  40160c:	bf 20 00 00 00       	mov    $0x20,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1587
    stats->gc_zct_mgmt_time = 0;
  401611:	48 c7 80 b0 00 00 00 	movq   $0x0,0xb0(%rax)
  401618:	00 00 00 00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1594
    stats->rootset_size = 0;
  40161c:	48 c7 80 e8 00 00 00 	movq   $0x0,0xe8(%rax)
  401623:	00 00 00 00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1565
    stats->minor_collections = 0;
  401627:	0f 11 00             	movups %xmm0,(%rax)
  40162a:	0f 11 40 10          	movups %xmm0,0x10(%rax)
  40162e:	0f 11 40 20          	movups %xmm0,0x20(%rax)
  401632:	0f 11 40 30          	movups %xmm0,0x30(%rax)
  401636:	0f 11 40 40          	movups %xmm0,0x40(%rax)
  40163a:	0f 11 40 50          	movups %xmm0,0x50(%rax)
  40163e:	0f 11 40 60          	movups %xmm0,0x60(%rax)
  401642:	0f 11 40 70          	movups %xmm0,0x70(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1581
    stats->gc_elapsed_time = 0;
  401646:	0f 11 88 80 00 00 00 	movups %xmm1,0x80(%rax)
  40164d:	0f 11 88 90 00 00 00 	movups %xmm1,0x90(%rax)
  401654:	0f 11 88 a0 00 00 00 	movups %xmm1,0xa0(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1588
    stats->fwd_env_size = 0;
  40165b:	0f 11 80 b8 00 00 00 	movups %xmm0,0xb8(%rax)
  401662:	0f 11 80 c8 00 00 00 	movups %xmm0,0xc8(%rax)
  401669:	0f 11 80 d8 00 00 00 	movups %xmm0,0xd8(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1338
    gib_global_gc_stats = (GibGcStats *) gib_alloc(sizeof(GibGcStats));
  401670:	48 89 05 e9 3a 00 00 	mov    %rax,0x3ae9(%rip)        # 405160 <gib_global_gc_stats>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401677:	e8 e4 fa ff ff       	call   401160 <malloc@plt>
  40167c:	bf 00 00 40 00       	mov    $0x400000,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1420
    nursery->heap_size = nsize;
  401681:	48 c7 00 00 00 40 00 	movq   $0x400000,(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401688:	48 89 c3             	mov    %rax,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1343
    gib_global_nurseries = (GibNursery *) gib_alloc(gib_global_num_threads *
  40168b:	48 89 05 ee 3a 00 00 	mov    %rax,0x3aee(%rip)        # 405180 <gib_global_nurseries>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401692:	e8 c9 fa ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1421
    nursery->heap_start = (char *) gib_alloc(nsize);
  401697:	48 89 43 08          	mov    %rax,0x8(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  40169b:	48 89 c6             	mov    %rax,%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1422
    if (nursery->heap_start == NULL) {
  40169e:	48 85 c0             	test   %rax,%rax
  4016a1:	0f 84 bc 08 00 00    	je     401f63 <main+0xd83>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1427
    nursery->heap_end = nursery->heap_start + nsize;
  4016a7:	48 8d 90 00 00 40 00 	lea    0x400000(%rax),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1431
    printf("Nursery info: start=%p, end=%p, alloc=%p, size=%zu\n\n",
  4016ae:	41 b8 00 00 40 00    	mov    $0x400000,%r8d
  4016b4:	bf 70 36 40 00       	mov    $0x403670,%edi
  4016b9:	31 c0                	xor    %eax,%eax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1427
    nursery->heap_end = nursery->heap_start + nsize;
  4016bb:	48 89 53 10          	mov    %rdx,0x10(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1431
    printf("Nursery info: start=%p, end=%p, alloc=%p, size=%zu\n\n",
  4016bf:	48 89 d1             	mov    %rdx,%rcx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1428
    nursery->alloc = nursery->heap_end;
  4016c2:	48 89 53 18          	mov    %rdx,0x18(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1431
    printf("Nursery info: start=%p, end=%p, alloc=%p, size=%zu\n\n",
  4016c6:	e8 f5 f9 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4016cb:	bf 18 00 00 00       	mov    $0x18,%edi
  4016d0:	e8 8b fa ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1454
    oldgen->old_zct = (void *) NULL;
  4016d5:	66 0f ef c0          	pxor   %xmm0,%xmm0
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4016d9:	bf 18 00 00 00       	mov    $0x18,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1454
    oldgen->old_zct = (void *) NULL;
  4016de:	0f 11 40 08          	movups %xmm0,0x8(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4016e2:	48 89 c5             	mov    %rax,%rbp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1350
    gib_global_oldgen = (GibOldgen *) gib_alloc(sizeof(GibOldgen));
  4016e5:	48 89 05 7c 3a 00 00 	mov    %rax,0x3a7c(%rip)        # 405168 <gib_global_oldgen>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4016ec:	e8 6f fa ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1457
    oldgen->rem_set = (GibRememberedSet *) gib_alloc(sizeof(GibRememberedSet));
  4016f1:	48 89 45 00          	mov    %rax,0x0(%rbp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4016f5:	48 89 c3             	mov    %rax,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1458
    if (oldgen->rem_set == NULL) {
  4016f8:	48 85 c0             	test   %rax,%rax
  4016fb:	0f 84 2b 09 00 00    	je     40202c <main+0xe4c>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401701:	bf 00 00 00 06       	mov    $0x6000000,%edi
  401706:	e8 55 fa ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1485
    stack->start = (char *) gib_alloc(stack_size);
  40170b:	48 89 03             	mov    %rax,(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1486
    if (stack->start == NULL) {
  40170e:	48 85 c0             	test   %rax,%rax
  401711:	0f 84 24 09 00 00    	je     40203b <main+0xe5b>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1491
    stack->end = stack->start + stack_size;
  401717:	48 8d 90 00 00 00 06 	lea    0x6000000(%rax),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1492
    stack->alloc = stack->start;
  40171e:	48 89 43 10          	mov    %rax,0x10(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401722:	bf 18 00 00 00       	mov    $0x18,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1491
    stack->end = stack->start + stack_size;
  401727:	48 89 53 08          	mov    %rdx,0x8(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  40172b:	e8 30 fa ff ff       	call   401160 <malloc@plt>
  401730:	bf 18 00 00 00       	mov    $0x18,%edi
  401735:	49 89 c5             	mov    %rax,%r13
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1355
    gib_global_read_shadowstacks =
  401738:	48 89 05 39 3a 00 00 	mov    %rax,0x3a39(%rip)        # 405178 <gib_global_read_shadowstacks>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  40173f:	e8 1c fa ff ff       	call   401160 <malloc@plt>
  401744:	bf 00 00 00 06       	mov    $0x6000000,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1358
    gib_global_write_shadowstacks =
  401749:	48 89 05 20 3a 00 00 	mov    %rax,0x3a20(%rip)        # 405170 <gib_global_write_shadowstacks>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401750:	48 89 c3             	mov    %rax,%rbx
  401753:	e8 08 fa ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1485
    stack->start = (char *) gib_alloc(stack_size);
  401758:	49 89 45 00          	mov    %rax,0x0(%r13)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1486
    if (stack->start == NULL) {
  40175c:	48 85 c0             	test   %rax,%rax
  40175f:	0f 84 d6 08 00 00    	je     40203b <main+0xe5b>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1491
    stack->end = stack->start + stack_size;
  401765:	48 8d 90 00 00 00 06 	lea    0x6000000(%rax),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1492
    stack->alloc = stack->start;
  40176c:	49 89 45 10          	mov    %rax,0x10(%r13)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401770:	bf 00 00 00 06       	mov    $0x6000000,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1491
    stack->end = stack->start + stack_size;
  401775:	49 89 55 08          	mov    %rdx,0x8(%r13)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401779:	e8 e2 f9 ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1485
    stack->start = (char *) gib_alloc(stack_size);
  40177e:	48 89 03             	mov    %rax,(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1486
    if (stack->start == NULL) {
  401781:	48 85 c0             	test   %rax,%rax
  401784:	0f 84 b1 08 00 00    	je     40203b <main+0xe5b>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1491
    stack->end = stack->start + stack_size;
  40178a:	48 8d 90 00 00 00 06 	lea    0x6000000(%rax),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1492
    stack->alloc = stack->start;
  401791:	48 89 43 10          	mov    %rax,0x10(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:2014

#ifndef _GIBBON_POINTER
    // Initialize the nursery and shadow stack.
    gib_storage_initialize();
    GibOldgen *oldgen = DEFAULT_GENERATION;
    gib_init_zcts(oldgen);
  401795:	48 89 ef             	mov    %rbp,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1491
    stack->end = stack->start + stack_size;
  401798:	48 89 53 08          	mov    %rdx,0x8(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:2014
    gib_init_zcts(oldgen);
  40179c:	e8 cf f8 ff ff       	call   401070 <gib_init_zcts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:2017

    // Minimal test to see if FFI is set up correctly.
    gib_check_rust_struct_sizes();
  4017a1:	e8 8a 15 00 00       	call   402d30 <gib_check_rust_struct_sizes>
info_table_initialize():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:87
    int error = gib_info_table_initialize(7);
  4017a6:	bf 07 00 00 00       	mov    $0x7,%edi
  4017ab:	e8 40 f9 ff ff       	call   4010f0 <gib_info_table_initialize@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:89
    if (error < 0) {
  4017b0:	85 c0                	test   %eax,%eax
  4017b2:	0f 88 f4 07 00 00    	js     401fac <main+0xdcc>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:96
    gib_info_table_finalize();
  4017b8:	e8 e3 f9 ff ff       	call   4011a0 <gib_info_table_finalize@plt>
symbol_table_initialize():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:100
    gib_add_symbol(1236, "]");
  4017bd:	be 19 37 40 00       	mov    $0x403719,%esi
  4017c2:	bf d4 04 00 00       	mov    $0x4d4,%edi
  4017c7:	41 bd 00 00 00 00    	mov    $0x0,%r13d
  4017cd:	e8 be 11 00 00       	call   402990 <gib_add_symbol>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:101
    gib_add_symbol(1237, "[");
  4017d2:	be 0f 37 40 00       	mov    $0x40370f,%esi
  4017d7:	bf d5 04 00 00       	mov    $0x4d5,%edi
  4017dc:	e8 af 11 00 00       	call   402990 <gib_add_symbol>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:102
    gib_add_symbol(1238, ",");
  4017e1:	be 75 38 40 00       	mov    $0x403875,%esi
  4017e6:	bf d6 04 00 00       	mov    $0x4d6,%edi
  4017eb:	e8 a0 11 00 00       	call   402990 <gib_add_symbol>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:78
    return gib_global_size_param;
  4017f0:	4c 8b 35 09 39 00 00 	mov    0x3909(%rip),%r14        # 405100 <gib_global_size_param>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:108
    if (fltIf_1019_1066) {
  4017f7:	4d 85 f6             	test   %r14,%r14
  4017fa:	4d 0f 49 ee          	cmovns %r14,%r13
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:415
    
    GibInt n_42_859_1054 = gib_get_size_param();
    GibInt n_100_892_961_1055 = gib_get_size_param();
    GibInt n__103_894_963_1057 =  maxInt(n_100_892_961_1055, 0);
    GibInt tmp_19 = sizeof(GibInt);
    GibVector *vec_104_895_964_1058 = gib_vector_alloc(n__103_894_963_1057,
  4017fe:	4c 89 ef             	mov    %r13,%rdi
  401801:	e8 2a 0f 00 00       	call   402730 <gib_vector_alloc.constprop.0>
  401806:	48 89 c5             	mov    %rax,%rbp
generate_loop_544_803():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:154
    if (fltIf_1023_1091) {
  401809:	4d 85 f6             	test   %r14,%r14
  40180c:	7e 13                	jle    401821 <main+0x641>
  40180e:	4c 89 f1             	mov    %r14,%rcx
  401811:	4c 89 ea             	mov    %r13,%rdx
  401814:	31 f6                	xor    %esi,%esi
  401816:	48 89 c7             	mov    %rax,%rdi
  401819:	e8 02 0b 00 00       	call   402320 <generate_loop_544_803.part.0>
  40181e:	48 89 c5             	mov    %rax,%rbp
main():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:420
                                                       tmp_19);
    GibVector *vec1_105_896_965_1059 =
               generate_loop_544_803(vec_104_895_964_1058, 0, n__103_894_963_1057, n_42_859_1054);
    GibVector *timed_1232;
    GibVector *times_12 = gib_vector_alloc(gib_get_iters_param(),
  401821:	48 8b 3d d0 38 00 00 	mov    0x38d0(%rip),%rdi        # 4050f8 <gib_global_iters_param>
  401828:	e8 03 0f 00 00       	call   402730 <gib_vector_alloc.constprop.0>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:425
                                           sizeof(double));
    struct timespec begin_timed_1232;
    struct timespec end_timed_1232;
    
    for (long long iters_timed_1232 = 0; iters_timed_1232 <
  40182d:	48 83 3d c3 38 00 00 	cmpq   $0x0,0x38c3(%rip)        # 4050f8 <gib_global_iters_param>
  401834:	00 
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:420
    GibVector *times_12 = gib_vector_alloc(gib_get_iters_param(),
  401835:	49 89 c5             	mov    %rax,%r13
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:425
    for (long long iters_timed_1232 = 0; iters_timed_1232 <
  401838:	0f 8e d1 01 00 00    	jle    401a0f <main+0x82f>
  40183e:	31 db                	xor    %ebx,%ebx
  401840:	e9 a9 00 00 00       	jmp    4018ee <main+0x70e>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401845:	48 8b 45 00          	mov    0x0(%rbp),%rax
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:254
            GibInt fltAppE_1037_1143 = *tmp_4;
  401849:	48 8b 55 18          	mov    0x18(%rbp),%rdx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:256
                       insert_541_809(xs__66_943_1140, fltAppE_1037_1143, n_63_941_1134);
  40184d:	4c 89 ff             	mov    %r15,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  401850:	4c 01 e0             	add    %r12,%rax
  401853:	48 0f af 45 10       	imul   0x10(%rbp),%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:256
  401858:	48 8b 34 02          	mov    (%rdx,%rax,1),%rsi
  40185c:	4c 89 e2             	mov    %r12,%rdx
  40185f:	e8 fc 09 00 00       	call   402260 <insert_541_809>
  401864:	49 89 c4             	mov    %rax,%r12
main():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:436
        clock_gettime(CLOCK_MONOTONIC_RAW, &begin_timed_1232);
        
        GibVector *tailapp_1229 =  isort1_533_796(vec1_105_896_965_1059);
        
        timed_1232 = tailapp_1229;
        clock_gettime(CLOCK_MONOTONIC_RAW, &end_timed_1232);
  401867:	48 8d 74 24 60       	lea    0x60(%rsp),%rsi
  40186c:	bf 04 00 00 00       	mov    $0x4,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401871:	4c 8d 7c 24 50       	lea    0x50(%rsp),%r15
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:436
  401876:	e8 25 f8 ff ff       	call   4010a0 <clock_gettime@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1828
            + ((double)(t1->tv_nsec - t0->tv_nsec) / 1000000000.0);
  40187b:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401880:	66 0f ef c0          	pxor   %xmm0,%xmm0
  401884:	48 2b 44 24 78       	sub    0x78(%rsp),%rax
  401889:	f2 48 0f 2a c0       	cvtsi2sd %rax,%xmm0
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1827
    return (double)(t1->tv_sec - t0->tv_sec)
  40188e:	66 0f ef c9          	pxor   %xmm1,%xmm1
  401892:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401897:	48 2b 44 24 70       	sub    0x70(%rsp),%rax
  40189c:	f2 48 0f 2a c8       	cvtsi2sd %rax,%xmm1
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:445
        }
        
        double itertime_9 = gib_difftimespecs(&begin_timed_1232,
                                              &end_timed_1232);
        
        printf("itertime: %lf\n", itertime_9);
  4018a1:	bf 77 38 40 00       	mov    $0x403877,%edi
  4018a6:	b8 01 00 00 00       	mov    $0x1,%eax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1828
            + ((double)(t1->tv_nsec - t0->tv_nsec) / 1000000000.0);
  4018ab:	f2 0f 5e 05 45 20 00 	divsd  0x2045(%rip),%xmm0        # 4038f8 <__PRETTY_FUNCTION__.3+0x28>
  4018b2:	00 
  4018b3:	f2 0f 58 c1          	addsd  %xmm1,%xmm0
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:442
        double itertime_9 = gib_difftimespecs(&begin_timed_1232,
  4018b7:	f2 0f 11 44 24 50    	movsd  %xmm0,0x50(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:445
        printf("itertime: %lf\n", itertime_9);
  4018bd:	e8 fe f7 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4018c2:	49 8b 7d 00          	mov    0x0(%r13),%rdi
  4018c6:	49 8b 55 10          	mov    0x10(%r13),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4018ca:	4c 89 fe             	mov    %r15,%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4018cd:	48 01 df             	add    %rbx,%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:426
         gib_get_iters_param(); iters_timed_1232++) {
  4018d0:	48 83 c3 01          	add    $0x1,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4018d4:	48 0f af fa          	imul   %rdx,%rdi
  4018d8:	49 03 7d 18          	add    0x18(%r13),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4018dc:	e8 5f f8 ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:425
    for (long long iters_timed_1232 = 0; iters_timed_1232 <
  4018e1:	48 3b 1d 10 38 00 00 	cmp    0x3810(%rip),%rbx        # 4050f8 <gib_global_iters_param>
  4018e8:	0f 8d 21 01 00 00    	jge    401a0f <main+0x82f>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:431
        clock_gettime(CLOCK_MONOTONIC_RAW, &begin_timed_1232);
  4018ee:	48 8d 74 24 70       	lea    0x70(%rsp),%rsi
  4018f3:	bf 04 00 00 00       	mov    $0x4,%edi
  4018f8:	41 be 00 00 00 00    	mov    $0x0,%r14d
  4018fe:	e8 9d f7 ff ff       	call   4010a0 <clock_gettime@plt>
isort1_533_796():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:195
    GibInt n_56_931_1109 = gib_vector_length(xs_54_930_1107);
  401903:	48 8b 45 00          	mov    0x0(%rbp),%rax
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401907:	48 8b 55 10          	mov    0x10(%rbp),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  40190b:	bf 20 00 00 00       	mov    $0x20,%edi
isort1_533_796():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:200
    GibInt hd_57_932_1112 = *tmp_3;
  401910:	48 8b 4d 18          	mov    0x18(%rbp),%rcx
  401914:	4c 8b 65 08          	mov    0x8(%rbp),%r12
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401918:	48 0f af d0          	imul   %rax,%rdx
  40191c:	49 29 c4             	sub    %rax,%r12
  40191f:	4d 0f 49 f4          	cmovns %r12,%r14
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:200
  401923:	48 8b 14 11          	mov    (%rcx,%rdx,1),%rdx
  401927:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  40192c:	e8 2f f8 ff ff       	call   401160 <malloc@plt>
  401931:	49 89 c7             	mov    %rax,%r15
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:515
    if (vec == NULL) {
  401934:	48 85 c0             	test   %rax,%rax
  401937:	0f 84 04 06 00 00    	je     401f41 <main+0xd61>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:519
    void *data = (void *) gib_alloc(num * elt_size);
  40193d:	4a 8d 3c f5 00 00 00 	lea    0x0(,%r14,8),%rdi
  401944:	00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401945:	e8 16 f8 ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:520
    if (data == NULL) {
  40194a:	48 85 c0             	test   %rax,%rax
  40194d:	0f 84 ce 05 00 00    	je     401f21 <main+0xd41>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:524
    vec->lower = 0;
  401953:	49 c7 07 00 00 00 00 	movq   $0x0,(%r15)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:525
    vec->upper = num;
  40195a:	4d 89 77 08          	mov    %r14,0x8(%r15)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:526
    vec->elt_size = elt_size;
  40195e:	49 c7 47 10 08 00 00 	movq   $0x8,0x10(%r15)
  401965:	00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:527
    vec->data = data;
  401966:	49 89 47 18          	mov    %rax,0x18(%r15)
generate_loop_544_808():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:220
    if (fltIf_1031_1126) {
  40196a:	4d 85 e4             	test   %r12,%r12
  40196d:	7e 49                	jle    4019b8 <main+0x7d8>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  40196f:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  401974:	48 89 10             	mov    %rdx,(%rax)
generate_loop_544_808():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:220
  401977:	49 83 fc 01          	cmp    $0x1,%r12
  40197b:	74 3b                	je     4019b8 <main+0x7d8>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
  40197d:	48 89 50 08          	mov    %rdx,0x8(%rax)
generate_loop_544_808():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:220
  401981:	49 83 fc 02          	cmp    $0x2,%r12
  401985:	74 31                	je     4019b8 <main+0x7d8>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
  401987:	48 89 50 10          	mov    %rdx,0x10(%rax)
generate_loop_544_808():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:220
  40198b:	49 83 fc 03          	cmp    $0x3,%r12
  40198f:	74 27                	je     4019b8 <main+0x7d8>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
  401991:	48 89 50 18          	mov    %rdx,0x18(%rax)
generate_loop_544_808():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:220
  401995:	49 83 fc 04          	cmp    $0x4,%r12
  401999:	74 1d                	je     4019b8 <main+0x7d8>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
  40199b:	48 89 50 20          	mov    %rdx,0x20(%rax)
generate_loop_544_808():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:228
                   generate_loop_544_808(vec1_259_938_1130, fltAppE_1033_1131, end_256_936_1124, hd_57_937_1125);
  40199f:	48 89 d1             	mov    %rdx,%rcx
  4019a2:	4c 89 ff             	mov    %r15,%rdi
  4019a5:	4c 89 f2             	mov    %r14,%rdx
  4019a8:	be 05 00 00 00       	mov    $0x5,%esi
  4019ad:	e8 ae 07 00 00       	call   402160 <generate_loop_544_808>
  4019b2:	49 89 c7             	mov    %rax,%r15
  4019b5:	0f 1f 00             	nopl   (%rax)
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:533
    return (vec->upper - vec->lower);
  4019b8:	48 8b 55 08          	mov    0x8(%rbp),%rdx
  4019bc:	48 2b 55 00          	sub    0x0(%rbp),%rdx
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:239
    if (fltIf_1034_1137) {
  4019c0:	49 89 ec             	mov    %rbp,%r12
  4019c3:	48 83 fa 01          	cmp    $0x1,%rdx
  4019c7:	0f 8e 9a fe ff ff    	jle    401867 <main+0x687>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:244
        if (fltIf_1035_1138) {
  4019cd:	49 89 d6             	mov    %rdx,%r14
isort1_533_796():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:208
    GibInt fltAppE_1029_1121 = fltPrm_1030_1120 - 1;
  4019d0:	4c 8d 62 ff          	lea    -0x1(%rdx),%r12
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:244
        if (fltIf_1035_1138) {
  4019d4:	49 83 ee 02          	sub    $0x2,%r14
  4019d8:	0f 84 67 fe ff ff    	je     401845 <main+0x665>
  4019de:	48 83 ea 03          	sub    $0x3,%rdx
  4019e2:	0f 85 fb 04 00 00    	jne    401ee3 <main+0xd03>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4019e8:	48 8b 45 00          	mov    0x0(%rbp),%rax
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:254
            GibInt fltAppE_1037_1143 = *tmp_4;
  4019ec:	48 8b 55 18          	mov    0x18(%rbp),%rdx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:256
                       insert_541_809(xs__66_943_1140, fltAppE_1037_1143, n_63_941_1134);
  4019f0:	4c 89 ff             	mov    %r15,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4019f3:	4c 01 f0             	add    %r14,%rax
  4019f6:	48 0f af 45 10       	imul   0x10(%rbp),%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:256
  4019fb:	48 8b 34 02          	mov    (%rdx,%rax,1),%rsi
  4019ff:	4c 89 f2             	mov    %r14,%rdx
  401a02:	e8 59 08 00 00       	call   402260 <insert_541_809>
  401a07:	49 89 c7             	mov    %rax,%r15
  401a0a:	e9 36 fe ff ff       	jmp    401845 <main+0x665>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  401a0f:	49 8b 55 10          	mov    0x10(%r13),%rdx
  401a13:	49 8b 7d 00          	mov    0x0(%r13),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:599
    qsort(start, gib_vector_length(vec), vec->elt_size, compar);
  401a17:	b9 40 21 40 00       	mov    $0x402140,%ecx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:533
    return (vec->upper - vec->lower);
  401a1c:	49 8b 75 08          	mov    0x8(%r13),%rsi
  401a20:	48 29 fe             	sub    %rdi,%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401a23:	48 0f af fa          	imul   %rdx,%rdi
  401a27:	49 03 7d 18          	add    0x18(%r13),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:599
    qsort(start, gib_vector_length(vec), vec->elt_size, compar);
  401a2b:	e8 60 f6 ff ff       	call   401090 <qsort@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:450
        gib_vector_inplace_update(times_12, iters_timed_1232, &itertime_9);
    }
    gib_vector_inplace_sort(times_12, gib_compare_doubles);
    
    double *tmp_13 = (double *) gib_vector_nth(times_12, gib_get_iters_param() /
  401a30:	48 8b 05 c1 36 00 00 	mov    0x36c1(%rip),%rax        # 4050f8 <gib_global_iters_param>
  401a37:	41 b8 02 00 00 00    	mov    $0x2,%r8d
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401a3d:	49 8b 7d 00          	mov    0x0(%r13),%rdi
  401a41:	49 8b 75 10          	mov    0x10(%r13),%rsi
  401a45:	49 8b 4d 18          	mov    0x18(%r13),%rcx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:450
  401a49:	48 99                	cqto
  401a4b:	49 f7 f8             	idiv   %r8
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:533
    return (vec->upper - vec->lower);
  401a4e:	49 8b 55 08          	mov    0x8(%r13),%rdx
  401a52:	48 29 fa             	sub    %rdi,%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401a55:	48 01 f8             	add    %rdi,%rax
  401a58:	48 0f af c6          	imul   %rsi,%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:452
                                               2);
    double selftimed_11 = *tmp_13;
  401a5c:	f2 0f 10 0c 01       	movsd  (%rcx,%rax,1),%xmm1
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:694
    for(int i = 0; i < gib_vector_length(times); i++) {
  401a61:	48 85 d2             	test   %rdx,%rdx
  401a64:	0f 8e 95 04 00 00    	jle    401eff <main+0xd1f>
  401a6a:	48 0f af fe          	imul   %rsi,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:693
    double acc = 0;
  401a6e:	66 0f ef d2          	pxor   %xmm2,%xmm2
  401a72:	f2 0f 11 54 24 20    	movsd  %xmm2,0x20(%rsp)
  401a78:	66 0f 28 c2          	movapd %xmm2,%xmm0
  401a7c:	48 8d 04 39          	lea    (%rcx,%rdi,1),%rax
  401a80:	89 d1                	mov    %edx,%ecx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:694
    for(int i = 0; i < gib_vector_length(times); i++) {
  401a82:	31 d2                	xor    %edx,%edx
  401a84:	0f 1f 40 00          	nopl   0x0(%rax)
  401a88:	83 c2 01             	add    $0x1,%edx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:696
        acc += *d;
  401a8b:	f2 0f 58 00          	addsd  (%rax),%xmm0
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:694
    for(int i = 0; i < gib_vector_length(times); i++) {
  401a8f:	48 01 f0             	add    %rsi,%rax
  401a92:	39 ca                	cmp    %ecx,%edx
  401a94:	75 f2                	jne    401a88 <main+0x8a8>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:455
    double batchtime_10 = gib_sum_timing_array(times_12);
    
    gib_print_timing_array(times_12);
  401a96:	4c 89 ef             	mov    %r13,%rdi
  401a99:	f2 0f 11 4c 24 08    	movsd  %xmm1,0x8(%rsp)
  401a9f:	f2 0f 11 44 24 18    	movsd  %xmm0,0x18(%rsp)
  401aa5:	e8 56 0d 00 00       	call   402800 <gib_print_timing_array>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:242
void gib_free(void *ptr) { free(ptr); }
  401aaa:	49 8b 7d 18          	mov    0x18(%r13),%rdi
  401aae:	e8 7d f5 ff ff       	call   401030 <free@plt>
  401ab3:	4c 89 ef             	mov    %r13,%rdi
  401ab6:	e8 75 f5 ff ff       	call   401030 <free@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:457
    gib_vector_free(times_12);
    printf("ITERS: %ld\n", gib_get_iters_param());
  401abb:	48 8b 35 36 36 00 00 	mov    0x3636(%rip),%rsi        # 4050f8 <gib_global_iters_param>
  401ac2:	bf 86 38 40 00       	mov    $0x403886,%edi
  401ac7:	31 c0                	xor    %eax,%eax
  401ac9:	e8 f2 f5 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:458
    printf("SIZE: %ld\n", gib_get_size_param());
  401ace:	48 8b 35 2b 36 00 00 	mov    0x362b(%rip),%rsi        # 405100 <gib_global_size_param>
  401ad5:	bf 92 38 40 00       	mov    $0x403892,%edi
  401ada:	31 c0                	xor    %eax,%eax
  401adc:	e8 df f5 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:459
    printf("BATCHTIME: %e\n", batchtime_10);
  401ae1:	f2 0f 10 44 24 18    	movsd  0x18(%rsp),%xmm0
  401ae7:	bf 9d 38 40 00       	mov    $0x40389d,%edi
  401aec:	b8 01 00 00 00       	mov    $0x1,%eax
  401af1:	e8 ca f5 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:460
    printf("SELFTIMED: %e\n", selftimed_11);
  401af6:	f2 0f 10 4c 24 08    	movsd  0x8(%rsp),%xmm1
  401afc:	bf ac 38 40 00       	mov    $0x4038ac,%edi
  401b01:	b8 01 00 00 00       	mov    $0x1,%eax
  401b06:	66 0f 28 c1          	movapd %xmm1,%xmm0
  401b0a:	e8 b1 f5 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:463
    
    GibVector *timed_1233;
    GibVector *times_17 = gib_vector_alloc(gib_get_iters_param(),
  401b0f:	48 8b 3d e2 35 00 00 	mov    0x35e2(%rip),%rdi        # 4050f8 <gib_global_iters_param>
  401b16:	e8 15 0c 00 00       	call   402730 <gib_vector_alloc.constprop.0>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:468
                                           sizeof(double));
    struct timespec begin_timed_1233;
    struct timespec end_timed_1233;
    
    for (long long iters_timed_1233 = 0; iters_timed_1233 <
  401b1b:	48 83 3d d5 35 00 00 	cmpq   $0x0,0x35d5(%rip)        # 4050f8 <gib_global_iters_param>
  401b22:	00 
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:463
    GibVector *times_17 = gib_vector_alloc(gib_get_iters_param(),
  401b23:	49 89 c5             	mov    %rax,%r13
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:468
    for (long long iters_timed_1233 = 0; iters_timed_1233 <
  401b26:	0f 8e c9 01 00 00    	jle    401cf5 <main+0xb15>
  401b2c:	48 c7 44 24 08 00 00 	movq   $0x0,0x8(%rsp)
  401b33:	00 00 
  401b35:	4c 8d 7c 24 50       	lea    0x50(%rsp),%r15
  401b3a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:474
         gib_get_iters_param(); iters_timed_1233++) {
        if (iters_timed_1233 != gib_get_iters_param() - 1) {
            gib_list_bumpalloc_save_state();
            gib_ptr_bumpalloc_save_state();
        }
        clock_gettime(CLOCK_MONOTONIC_RAW, &begin_timed_1233);
  401b40:	bf 04 00 00 00       	mov    $0x4,%edi
  401b45:	4c 89 fe             	mov    %r15,%rsi
  401b48:	41 be 00 00 00 00    	mov    $0x0,%r14d
  401b4e:	e8 4d f5 ff ff       	call   4010a0 <clock_gettime@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:533
    return (vec->upper - vec->lower);
  401b53:	48 8b 5d 08          	mov    0x8(%rbp),%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401b57:	bf 20 00 00 00       	mov    $0x20,%edi
  401b5c:	48 2b 5d 00          	sub    0x0(%rbp),%rbx
  401b60:	4c 0f 49 f3          	cmovns %rbx,%r14
  401b64:	e8 f7 f5 ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:515
    if (vec == NULL) {
  401b69:	48 85 c0             	test   %rax,%rax
  401b6c:	0f 84 cf 03 00 00    	je     401f41 <main+0xd61>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:519
    void *data = (void *) gib_alloc(num * elt_size);
  401b72:	4a 8d 3c f5 00 00 00 	lea    0x0(,%r14,8),%rdi
  401b79:	00 
  401b7a:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401b7f:	e8 dc f5 ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:520
    if (data == NULL) {
  401b84:	4c 8b 44 24 18       	mov    0x18(%rsp),%r8
  401b89:	48 85 c0             	test   %rax,%rax
  401b8c:	0f 84 8f 03 00 00    	je     401f21 <main+0xd41>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:524
    vec->lower = 0;
  401b92:	49 c7 00 00 00 00 00 	movq   $0x0,(%r8)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:525
    vec->upper = num;
  401b99:	4d 89 70 08          	mov    %r14,0x8(%r8)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:526
    vec->elt_size = elt_size;
  401b9d:	49 c7 40 10 08 00 00 	movq   $0x8,0x10(%r8)
  401ba4:	00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:527
    vec->data = data;
  401ba5:	49 89 40 18          	mov    %rax,0x18(%r8)
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:175
    if (fltIf_1026_1101) {
  401ba9:	48 85 db             	test   %rbx,%rbx
  401bac:	0f 8e 96 00 00 00    	jle    401c48 <main+0xa68>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401bb2:	48 8b 55 10          	mov    0x10(%rbp),%rdx
  401bb6:	48 0f af 55 00       	imul   0x0(%rbp),%rdx
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
        GibInt fltPrm_1027_1104 = *tmp_1;
  401bbb:	48 8b 4d 18          	mov    0x18(%rbp),%rcx
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401bbf:	48 8b 14 11          	mov    (%rcx,%rdx,1),%rdx
  401bc3:	48 89 10             	mov    %rdx,(%rax)
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:175
    if (fltIf_1026_1101) {
  401bc6:	48 83 fb 01          	cmp    $0x1,%rbx
  401bca:	0f 84 97 00 00 00    	je     401c67 <main+0xa87>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401bd0:	48 8b 55 00          	mov    0x0(%rbp),%rdx
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
        GibInt fltPrm_1027_1104 = *tmp_1;
  401bd4:	48 8b 4d 18          	mov    0x18(%rbp),%rcx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  401bd8:	48 83 c2 01          	add    $0x1,%rdx
  401bdc:	48 0f af 55 10       	imul   0x10(%rbp),%rdx
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401be1:	48 8b 14 11          	mov    (%rcx,%rdx,1),%rdx
  401be5:	48 89 50 08          	mov    %rdx,0x8(%rax)
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:175
    if (fltIf_1026_1101) {
  401be9:	48 83 fb 02          	cmp    $0x2,%rbx
  401bed:	74 59                	je     401c48 <main+0xa68>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401bef:	48 8b 55 00          	mov    0x0(%rbp),%rdx
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
        GibInt fltPrm_1027_1104 = *tmp_1;
  401bf3:	48 8b 4d 18          	mov    0x18(%rbp),%rcx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  401bf7:	48 83 c2 02          	add    $0x2,%rdx
  401bfb:	48 0f af 55 10       	imul   0x10(%rbp),%rdx
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401c00:	48 8b 14 11          	mov    (%rcx,%rdx,1),%rdx
  401c04:	48 89 50 10          	mov    %rdx,0x10(%rax)
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:175
    if (fltIf_1026_1101) {
  401c08:	48 83 fb 03          	cmp    $0x3,%rbx
  401c0c:	74 3a                	je     401c48 <main+0xa68>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401c0e:	48 8b 55 00          	mov    0x0(%rbp),%rdx
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
        GibInt fltPrm_1027_1104 = *tmp_1;
  401c12:	48 8b 4d 18          	mov    0x18(%rbp),%rcx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  401c16:	48 83 c2 03          	add    $0x3,%rdx
  401c1a:	48 0f af 55 10       	imul   0x10(%rbp),%rdx
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401c1f:	48 8b 14 11          	mov    (%rcx,%rdx,1),%rdx
  401c23:	48 89 50 18          	mov    %rdx,0x18(%rax)
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:175
    if (fltIf_1026_1101) {
  401c27:	48 83 fb 04          	cmp    $0x4,%rbx
  401c2b:	74 1b                	je     401c48 <main+0xa68>
  401c2d:	4c 89 c7             	mov    %r8,%rdi
  401c30:	48 89 e9             	mov    %rbp,%rcx
  401c33:	4c 89 f2             	mov    %r14,%rdx
  401c36:	be 04 00 00 00       	mov    $0x4,%esi
  401c3b:	e8 e0 07 00 00       	call   402420 <generate_loop_544_804.part.0>
  401c40:	49 89 c0             	mov    %rax,%r8
  401c43:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
main():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:468
    for (long long iters_timed_1233 = 0; iters_timed_1233 <
  401c48:	41 be 01 00 00 00    	mov    $0x1,%r14d
  401c4e:	66 90                	xchg   %ax,%ax
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:468
  401c50:	4c 89 c6             	mov    %r8,%rsi
  401c53:	4c 89 f7             	mov    %r14,%rdi
go_538_810():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:341
        GibInt fltAppE_1047_1179 = i_82_951_1174 + 1;
  401c56:	49 83 c6 01          	add    $0x1,%r14
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:341
  401c5a:	e8 51 0a 00 00       	call   4026b0 <shift_543_811.part.0>
  401c5f:	49 89 c0             	mov    %rax,%r8
go_538_810():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
    if (fltIf_1046_1177) {
  401c62:	49 39 de             	cmp    %rbx,%r14
  401c65:	75 e9                	jne    401c50 <main+0xa70>
main():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:479
        
        GibVector *tailapp_1230 =  isort2_534_797(vec1_105_896_965_1059);
        
        timed_1233 = tailapp_1230;
        clock_gettime(CLOCK_MONOTONIC_RAW, &end_timed_1233);
  401c67:	48 8d 74 24 40       	lea    0x40(%rsp),%rsi
  401c6c:	bf 04 00 00 00       	mov    $0x4,%edi
  401c71:	e8 2a f4 ff ff       	call   4010a0 <clock_gettime@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1828
            + ((double)(t1->tv_nsec - t0->tv_nsec) / 1000000000.0);
  401c76:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401c7b:	66 0f ef c0          	pxor   %xmm0,%xmm0
  401c7f:	48 2b 44 24 58       	sub    0x58(%rsp),%rax
  401c84:	f2 48 0f 2a c0       	cvtsi2sd %rax,%xmm0
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1827
    return (double)(t1->tv_sec - t0->tv_sec)
  401c89:	66 0f ef c9          	pxor   %xmm1,%xmm1
  401c8d:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  401c92:	48 2b 44 24 50       	sub    0x50(%rsp),%rax
  401c97:	f2 48 0f 2a c8       	cvtsi2sd %rax,%xmm1
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:488
        }
        
        double itertime_14 = gib_difftimespecs(&begin_timed_1233,
                                               &end_timed_1233);
        
        printf("itertime: %lf\n", itertime_14);
  401c9c:	bf 77 38 40 00       	mov    $0x403877,%edi
  401ca1:	b8 01 00 00 00       	mov    $0x1,%eax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1828
            + ((double)(t1->tv_nsec - t0->tv_nsec) / 1000000000.0);
  401ca6:	f2 0f 5e 05 4a 1c 00 	divsd  0x1c4a(%rip),%xmm0        # 4038f8 <__PRETTY_FUNCTION__.3+0x28>
  401cad:	00 
  401cae:	f2 0f 58 c1          	addsd  %xmm1,%xmm0
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:485
        double itertime_14 = gib_difftimespecs(&begin_timed_1233,
  401cb2:	f2 0f 11 44 24 38    	movsd  %xmm0,0x38(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:488
        printf("itertime: %lf\n", itertime_14);
  401cb8:	e8 03 f4 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401cbd:	48 8b 5c 24 08       	mov    0x8(%rsp),%rbx
  401cc2:	49 8b 7d 00          	mov    0x0(%r13),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401cc6:	48 8d 74 24 38       	lea    0x38(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401ccb:	49 8b 55 10          	mov    0x10(%r13),%rdx
  401ccf:	48 01 df             	add    %rbx,%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:469
         gib_get_iters_param(); iters_timed_1233++) {
  401cd2:	48 83 c3 01          	add    $0x1,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  401cd6:	48 0f af fa          	imul   %rdx,%rdi
  401cda:	49 03 7d 18          	add    0x18(%r13),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401cde:	e8 5d f4 ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:468
    for (long long iters_timed_1233 = 0; iters_timed_1233 <
  401ce3:	48 3b 1d 0e 34 00 00 	cmp    0x340e(%rip),%rbx        # 4050f8 <gib_global_iters_param>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:469
         gib_get_iters_param(); iters_timed_1233++) {
  401cea:	48 89 5c 24 08       	mov    %rbx,0x8(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:468
    for (long long iters_timed_1233 = 0; iters_timed_1233 <
  401cef:	0f 8c 4b fe ff ff    	jl     401b40 <main+0x960>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401cf5:	49 8b 55 10          	mov    0x10(%r13),%rdx
  401cf9:	49 8b 7d 00          	mov    0x0(%r13),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:599
    qsort(start, gib_vector_length(vec), vec->elt_size, compar);
  401cfd:	b9 40 21 40 00       	mov    $0x402140,%ecx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:533
    return (vec->upper - vec->lower);
  401d02:	49 8b 75 08          	mov    0x8(%r13),%rsi
  401d06:	48 29 fe             	sub    %rdi,%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401d09:	48 0f af fa          	imul   %rdx,%rdi
  401d0d:	49 03 7d 18          	add    0x18(%r13),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:599
    qsort(start, gib_vector_length(vec), vec->elt_size, compar);
  401d11:	e8 7a f3 ff ff       	call   401090 <qsort@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:493
        gib_vector_inplace_update(times_17, iters_timed_1233, &itertime_14);
    }
    gib_vector_inplace_sort(times_17, gib_compare_doubles);
    
    double *tmp_18 = (double *) gib_vector_nth(times_17, gib_get_iters_param() /
  401d16:	48 8b 05 db 33 00 00 	mov    0x33db(%rip),%rax        # 4050f8 <gib_global_iters_param>
  401d1d:	41 b8 02 00 00 00    	mov    $0x2,%r8d
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401d23:	49 8b 7d 00          	mov    0x0(%r13),%rdi
  401d27:	49 8b 75 10          	mov    0x10(%r13),%rsi
  401d2b:	49 8b 4d 18          	mov    0x18(%r13),%rcx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:493
  401d2f:	48 99                	cqto
  401d31:	49 f7 f8             	idiv   %r8
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:533
    return (vec->upper - vec->lower);
  401d34:	49 8b 55 08          	mov    0x8(%r13),%rdx
  401d38:	48 29 fa             	sub    %rdi,%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401d3b:	48 01 f8             	add    %rdi,%rax
  401d3e:	48 0f af c6          	imul   %rsi,%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:495
                                               2);
    double selftimed_16 = *tmp_18;
  401d42:	f2 0f 10 0c 01       	movsd  (%rcx,%rax,1),%xmm1
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:694
    for(int i = 0; i < gib_vector_length(times); i++) {
  401d47:	48 85 d2             	test   %rdx,%rdx
  401d4a:	0f 8e a6 01 00 00    	jle    401ef6 <main+0xd16>
  401d50:	48 0f af fe          	imul   %rsi,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:693
    double acc = 0;
  401d54:	66 0f ef c0          	pxor   %xmm0,%xmm0
  401d58:	48 8d 04 39          	lea    (%rcx,%rdi,1),%rax
  401d5c:	0f 1f 40 00          	nopl   0x0(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:694
    for(int i = 0; i < gib_vector_length(times); i++) {
  401d60:	83 44 24 10 01       	addl   $0x1,0x10(%rsp)
  401d65:	8b 5c 24 10          	mov    0x10(%rsp),%ebx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:696
        acc += *d;
  401d69:	f2 0f 58 00          	addsd  (%rax),%xmm0
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:694
    for(int i = 0; i < gib_vector_length(times); i++) {
  401d6d:	48 01 f0             	add    %rsi,%rax
  401d70:	39 d3                	cmp    %edx,%ebx
  401d72:	75 ec                	jne    401d60 <main+0xb80>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:498
    double batchtime_15 = gib_sum_timing_array(times_17);
    
    gib_print_timing_array(times_17);
  401d74:	4c 89 ef             	mov    %r13,%rdi
  401d77:	f2 0f 11 4c 24 08    	movsd  %xmm1,0x8(%rsp)
printVec_loop_545_802():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:130
    if (fltIf_1021_1082) {
  401d7d:	31 db                	xor    %ebx,%ebx
  401d7f:	f2 0f 11 44 24 10    	movsd  %xmm0,0x10(%rsp)
main():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:498
    gib_print_timing_array(times_17);
  401d85:	e8 76 0a 00 00       	call   402800 <gib_print_timing_array>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:242
void gib_free(void *ptr) { free(ptr); }
  401d8a:	49 8b 7d 18          	mov    0x18(%r13),%rdi
  401d8e:	e8 9d f2 ff ff       	call   401030 <free@plt>
  401d93:	4c 89 ef             	mov    %r13,%rdi
  401d96:	e8 95 f2 ff ff       	call   401030 <free@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:500
    gib_vector_free(times_17);
    printf("ITERS: %ld\n", gib_get_iters_param());
  401d9b:	48 8b 35 56 33 00 00 	mov    0x3356(%rip),%rsi        # 4050f8 <gib_global_iters_param>
  401da2:	bf 86 38 40 00       	mov    $0x403886,%edi
  401da7:	31 c0                	xor    %eax,%eax
  401da9:	e8 12 f3 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:501
    printf("SIZE: %ld\n", gib_get_size_param());
  401dae:	48 8b 35 4b 33 00 00 	mov    0x334b(%rip),%rsi        # 405100 <gib_global_size_param>
  401db5:	bf 92 38 40 00       	mov    $0x403892,%edi
  401dba:	31 c0                	xor    %eax,%eax
  401dbc:	e8 ff f2 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:502
    printf("BATCHTIME: %e\n", batchtime_15);
  401dc1:	f2 0f 10 44 24 10    	movsd  0x10(%rsp),%xmm0
  401dc7:	bf 9d 38 40 00       	mov    $0x40389d,%edi
  401dcc:	b8 01 00 00 00       	mov    $0x1,%eax
  401dd1:	e8 ea f2 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:503
    printf("SELFTIMED: %e\n", selftimed_16);
  401dd6:	f2 0f 10 4c 24 08    	movsd  0x8(%rsp),%xmm1
  401ddc:	bf ac 38 40 00       	mov    $0x4038ac,%edi
  401de1:	b8 01 00 00 00       	mov    $0x1,%eax
  401de6:	66 0f 28 c1          	movapd %xmm1,%xmm0
  401dea:	e8 d1 f2 ff ff       	call   4010c0 <printf@plt>
printVec_535_799():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:116
    unsigned char wildcard__178_109_900_1070 = gib_print_symbol(1237);
  401def:	bf d5 04 00 00       	mov    $0x4d5,%edi
  401df4:	e8 67 10 00 00       	call   402e60 <gib_print_symbol.isra.0>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:533
    return (vec->upper - vec->lower);
  401df9:	49 8b 6c 24 08       	mov    0x8(%r12),%rbp
printVec_loop_545_802():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:130
    if (fltIf_1021_1082) {
  401dfe:	49 2b 2c 24          	sub    (%r12),%rbp
  401e02:	74 39                	je     401e3d <main+0xc5d>
  401e04:	0f 1f 40 00          	nopl   0x0(%rax)
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401e08:	49 8b 04 24          	mov    (%r12),%rax
printVec_loop_545_802():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:137
        GibInt i_47_891_973_1083 = *tmp_0;
  401e0c:	49 8b 54 24 18       	mov    0x18(%r12),%rdx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:138
        unsigned char wildcard__187_165_911_1084 = printf("%ld",
  401e11:	bf 3c 37 40 00       	mov    $0x40373c,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  401e16:	48 01 d8             	add    %rbx,%rax
  401e19:	49 0f af 44 24 10    	imul   0x10(%r12),%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:141
        GibInt fltAppE_1022_1086 = idx_160_908_1079 + 1;
  401e1f:	48 83 c3 01          	add    $0x1,%rbx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:138
        unsigned char wildcard__187_165_911_1084 = printf("%ld",
  401e23:	48 8b 34 02          	mov    (%rdx,%rax,1),%rsi
  401e27:	31 c0                	xor    %eax,%eax
  401e29:	e8 92 f2 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:140
        unsigned char wildcard__184_166_912_1085 = gib_print_symbol(1238);
  401e2e:	bf d6 04 00 00       	mov    $0x4d6,%edi
  401e33:	e8 28 10 00 00       	call   402e60 <gib_print_symbol.isra.0>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:130
    if (fltIf_1021_1082) {
  401e38:	48 39 eb             	cmp    %rbp,%rbx
  401e3b:	75 cb                	jne    401e08 <main+0xc28>
printVec_535_799():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:120
    unsigned char wildcard__173_111_902_1073 = gib_print_symbol(1236);
  401e3d:	bf d4 04 00 00       	mov    $0x4d4,%edi
  401e42:	e8 19 10 00 00       	call   402e60 <gib_print_symbol.isra.0>
main():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:507
    
    unsigned char wildcard__33_48_863_1063 =  printVec_535_799(timed_1232);
    
    printf("'#(");
  401e47:	bf bb 38 40 00       	mov    $0x4038bb,%edi
  401e4c:	31 c0                	xor    %eax,%eax
  401e4e:	e8 6d f2 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:508
    printf("<vector>");
  401e53:	bf bf 38 40 00       	mov    $0x4038bf,%edi
  401e58:	31 c0                	xor    %eax,%eax
  401e5a:	e8 61 f2 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:509
    printf(" ");
  401e5f:	bf 20 00 00 00       	mov    $0x20,%edi
  401e64:	e8 d7 f1 ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:510
    printf("<vector>");
  401e69:	bf bf 38 40 00       	mov    $0x4038bf,%edi
  401e6e:	31 c0                	xor    %eax,%eax
  401e70:	e8 4b f2 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:511
    printf(")");
  401e75:	bf 29 00 00 00       	mov    $0x29,%edi
  401e7a:	e8 c1 f1 ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:512
    printf("\n");
  401e7f:	bf 0a 00 00 00       	mov    $0xa,%edi
  401e84:	e8 b7 f1 ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:242
void gib_free(void *ptr) { free(ptr); }
  401e89:	48 8b 3d f8 32 00 00 	mov    0x32f8(%rip),%rdi        # 405188 <gib_global_bench_prog_param>
  401e90:	e8 9b f1 ff ff       	call   401030 <free@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:2042
    GibShadowstack *rstack = DEFAULT_READ_SHADOWSTACK;
    GibShadowstack *wstack = DEFAULT_WRITE_SHADOWSTACK;
    GibOldgen *oldgen = DEFAULT_GENERATION;

    // Free all objects initialized by the Rust RTS.
    gib_gc_cleanup(rstack, wstack, nursery, oldgen);
  401e95:	48 8b 0d cc 32 00 00 	mov    0x32cc(%rip),%rcx        # 405168 <gib_global_oldgen>
  401e9c:	48 8b 15 dd 32 00 00 	mov    0x32dd(%rip),%rdx        # 405180 <gib_global_nurseries>
  401ea3:	48 8b 35 c6 32 00 00 	mov    0x32c6(%rip),%rsi        # 405170 <gib_global_write_shadowstacks>
  401eaa:	48 8b 3d c7 32 00 00 	mov    0x32c7(%rip),%rdi        # 405178 <gib_global_read_shadowstacks>
  401eb1:	e8 1a f2 ff ff       	call   4010d0 <gib_gc_cleanup@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:517
    
    int exit_21 = gib_exit();
    
    return exit_21;
  401eb6:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  401ebd:	31 c0                	xor    %eax,%eax
  401ebf:	5b                   	pop    %rbx
  401ec0:	5d                   	pop    %rbp
  401ec1:	41 5c                	pop    %r12
  401ec3:	41 5d                	pop    %r13
  401ec5:	41 5e                	pop    %r14
  401ec7:	41 5f                	pop    %r15
  401ec9:	c3                   	ret
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401eca:	bf 01 00 00 00       	mov    $0x1,%edi
  401ecf:	e8 8c f2 ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1997
        gib_global_bench_prog_param = (char*) gib_alloc(1*sizeof(char));
  401ed4:	48 89 05 ad 32 00 00 	mov    %rax,0x32ad(%rip)        # 405188 <gib_global_bench_prog_param>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1998
        *gib_global_bench_prog_param = '\n';
  401edb:	c6 00 0a             	movb   $0xa,(%rax)
  401ede:	e9 06 f7 ff ff       	jmp    4015e9 <main+0x409>
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1998
  401ee3:	4c 89 fe             	mov    %r15,%rsi
  401ee6:	48 89 ef             	mov    %rbp,%rdi
  401ee9:	e8 62 06 00 00       	call   402550 <isort_540_807.part.0>
  401eee:	49 89 c7             	mov    %rax,%r15
  401ef1:	e9 f2 fa ff ff       	jmp    4019e8 <main+0x808>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:693
    double acc = 0;
  401ef6:	66 0f ef c0          	pxor   %xmm0,%xmm0
  401efa:	e9 75 fe ff ff       	jmp    401d74 <main+0xb94>
  401eff:	66 0f ef db          	pxor   %xmm3,%xmm3
  401f03:	f2 0f 11 5c 24 20    	movsd  %xmm3,0x20(%rsp)
  401f09:	66 0f 28 c3          	movapd %xmm3,%xmm0
  401f0d:	e9 84 fb ff ff       	jmp    401a96 <main+0x8b6>
  401f12:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  401f17:	be c1 37 40 00       	mov    $0x4037c1,%esi
  401f1c:	e8 3f 0a 00 00       	call   402960 <check_args.part.0>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:521
        fprintf(stderr, "alloc_vector: gib_alloc failed: %zu", sizeof(num * elt_size));
  401f21:	48 8b 3d 18 32 00 00 	mov    0x3218(%rip),%rdi        # 405140 <stderr@GLIBC_2.2.5>
  401f28:	ba 08 00 00 00       	mov    $0x8,%edx
  401f2d:	be 08 30 40 00       	mov    $0x403008,%esi
  401f32:	e8 f9 f1 ff ff       	call   401130 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:522
        exit(1);
  401f37:	bf 01 00 00 00       	mov    $0x1,%edi
  401f3c:	e8 6f f2 ff ff       	call   4011b0 <exit@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:516
        fprintf(stderr, "alloc_vector: gib_alloc failed: %zu", sizeof(GibVector));
  401f41:	ba 20 00 00 00       	mov    $0x20,%edx
  401f46:	be 08 30 40 00       	mov    $0x403008,%esi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1459
        fprintf(stderr, "gib_oldgen_initialize: gib_alloc failed: %zu",
  401f4b:	48 8b 3d ee 31 00 00 	mov    0x31ee(%rip),%rdi        # 405140 <stderr@GLIBC_2.2.5>
  401f52:	31 c0                	xor    %eax,%eax
  401f54:	e8 d7 f1 ff ff       	call   401130 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1461
        exit(1);
  401f59:	bf 01 00 00 00       	mov    $0x1,%edi
  401f5e:	e8 4d f2 ff ff       	call   4011b0 <exit@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1423
        fprintf(stderr, "gib_nursery_initialize: gib_alloc failed: %zu",
  401f63:	ba 00 00 40 00       	mov    $0x400000,%edx
  401f68:	be 40 36 40 00       	mov    $0x403640,%esi
  401f6d:	eb dc                	jmp    401f4b <main+0xd6b>
  401f6f:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  401f74:	be 26 38 40 00       	mov    $0x403826,%esi
  401f79:	e8 e2 09 00 00       	call   402960 <check_args.part.0>
  401f7e:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  401f83:	be d6 37 40 00       	mov    $0x4037d6,%esi
  401f88:	e8 d3 09 00 00       	call   402960 <check_args.part.0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1910
        fprintf(stderr, " [gibbon rts] failed to getrlimit, code %d\n", code);
  401f8d:	89 c2                	mov    %eax,%edx
  401f8f:	be 88 35 40 00       	mov    $0x403588,%esi
info_table_initialize():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:90
        fprintf(stderr, "Couldn't initialize info table, errorno=%d", error);
  401f94:	48 8b 3d a5 31 00 00 	mov    0x31a5(%rip),%rdi        # 405140 <stderr@GLIBC_2.2.5>
  401f9b:	31 c0                	xor    %eax,%eax
  401f9d:	e8 8e f1 ff ff       	call   401130 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:91
        exit(1);
  401fa2:	bf 01 00 00 00       	mov    $0x1,%edi
  401fa7:	e8 04 f2 ff ff       	call   4011b0 <exit@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:90
        fprintf(stderr, "Couldn't initialize info table, errorno=%d", error);
  401fac:	89 c2                	mov    %eax,%edx
  401fae:	be d8 36 40 00       	mov    $0x4036d8,%esi
  401fb3:	eb df                	jmp    401f94 <main+0xdb4>
  401fb5:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  401fba:	be 04 38 40 00       	mov    $0x403804,%esi
  401fbf:	e8 9c 09 00 00       	call   402960 <check_args.part.0>
  401fc4:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  401fc9:	be 30 38 40 00       	mov    $0x403830,%esi
  401fce:	e8 8d 09 00 00       	call   402960 <check_args.part.0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1987
            fprintf(stderr, "Extra arguments left over: ");
  401fd3:	48 8b 0d 66 31 00 00 	mov    0x3166(%rip),%rcx        # 405140 <stderr@GLIBC_2.2.5>
  401fda:	ba 1b 00 00 00       	mov    $0x1b,%edx
  401fdf:	be 01 00 00 00       	mov    $0x1,%esi
  401fe4:	4d 63 ff             	movslq %r15d,%r15
  401fe7:	bf 3d 38 40 00       	mov    $0x40383d,%edi
  401fec:	e8 cf f1 ff ff       	call   4011c0 <fwrite@plt>
  401ff1:	eb 20                	jmp    402013 <main+0xe33>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1988
            for(; i < argc; i++) fprintf(stderr, "%s ", argv[i]);
  401ff3:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  401ff8:	48 8b 3d 41 31 00 00 	mov    0x3141(%rip),%rdi        # 405140 <stderr@GLIBC_2.2.5>
  401fff:	be 59 38 40 00       	mov    $0x403859,%esi
  402004:	4a 8b 14 f8          	mov    (%rax,%r15,8),%rdx
  402008:	31 c0                	xor    %eax,%eax
  40200a:	49 83 c7 01          	add    $0x1,%r15
  40200e:	e8 1d f1 ff ff       	call   401130 <fprintf@plt>
  402013:	44 39 fb             	cmp    %r15d,%ebx
  402016:	7f db                	jg     401ff3 <main+0xe13>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1989
            gib_show_usage(argv);
  402018:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40201d:	e8 7e 08 00 00       	call   4028a0 <gib_show_usage>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1990
            exit(1);
  402022:	bf 01 00 00 00       	mov    $0x1,%edi
  402027:	e8 84 f1 ff ff       	call   4011b0 <exit@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1459
        fprintf(stderr, "gib_oldgen_initialize: gib_alloc failed: %zu",
  40202c:	ba 18 00 00 00       	mov    $0x18,%edx
  402031:	be a8 36 40 00       	mov    $0x4036a8,%esi
  402036:	e9 10 ff ff ff       	jmp    401f4b <main+0xd6b>
  40203b:	e8 90 07 00 00       	call   4027d0 <gib_shadowstack_initialize.part.0.constprop.0>
  402040:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  402045:	be 19 38 40 00       	mov    $0x403819,%esi
  40204a:	e8 11 09 00 00       	call   402960 <check_args.part.0>
main():
  40204f:	90                   	nop

0000000000402050 <_start>:
_start():
  402050:	f3 0f 1e fa          	endbr64
  402054:	31 ed                	xor    %ebp,%ebp
  402056:	49 89 d1             	mov    %rdx,%r9
  402059:	5e                   	pop    %rsi
  40205a:	48 89 e2             	mov    %rsp,%rdx
  40205d:	48 83 e4 f0          	and    $0xfffffffffffffff0,%rsp
  402061:	50                   	push   %rax
  402062:	54                   	push   %rsp
  402063:	45 31 c0             	xor    %r8d,%r8d
  402066:	31 c9                	xor    %ecx,%ecx
  402068:	48 c7 c7 e0 11 40 00 	mov    $0x4011e0,%rdi
  40206f:	ff 15 63 2f 00 00    	call   *0x2f63(%rip)        # 404fd8 <__libc_start_main@GLIBC_2.34>
  402075:	f4                   	hlt
  402076:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40207d:	00 00 00 

0000000000402080 <_dl_relocate_static_pie>:
_dl_relocate_static_pie():
  402080:	f3 0f 1e fa          	endbr64
  402084:	c3                   	ret
  402085:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40208c:	00 00 00 
  40208f:	90                   	nop

0000000000402090 <deregister_tm_clones>:
deregister_tm_clones():
  402090:	b8 08 51 40 00       	mov    $0x405108,%eax
  402095:	48 3d 08 51 40 00    	cmp    $0x405108,%rax
  40209b:	74 13                	je     4020b0 <deregister_tm_clones+0x20>
  40209d:	b8 00 00 00 00       	mov    $0x0,%eax
  4020a2:	48 85 c0             	test   %rax,%rax
  4020a5:	74 09                	je     4020b0 <deregister_tm_clones+0x20>
  4020a7:	bf 08 51 40 00       	mov    $0x405108,%edi
  4020ac:	ff e0                	jmp    *%rax
  4020ae:	66 90                	xchg   %ax,%ax
  4020b0:	c3                   	ret
  4020b1:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  4020b8:	00 00 00 00 
  4020bc:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004020c0 <register_tm_clones>:
register_tm_clones():
  4020c0:	be 08 51 40 00       	mov    $0x405108,%esi
  4020c5:	48 81 ee 08 51 40 00 	sub    $0x405108,%rsi
  4020cc:	48 89 f0             	mov    %rsi,%rax
  4020cf:	48 c1 ee 3f          	shr    $0x3f,%rsi
  4020d3:	48 c1 f8 03          	sar    $0x3,%rax
  4020d7:	48 01 c6             	add    %rax,%rsi
  4020da:	48 d1 fe             	sar    $1,%rsi
  4020dd:	74 11                	je     4020f0 <register_tm_clones+0x30>
  4020df:	b8 00 00 00 00       	mov    $0x0,%eax
  4020e4:	48 85 c0             	test   %rax,%rax
  4020e7:	74 07                	je     4020f0 <register_tm_clones+0x30>
  4020e9:	bf 08 51 40 00       	mov    $0x405108,%edi
  4020ee:	ff e0                	jmp    *%rax
  4020f0:	c3                   	ret
  4020f1:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  4020f8:	00 00 00 00 
  4020fc:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402100 <__do_global_dtors_aux>:
__do_global_dtors_aux():
  402100:	80 3d 41 30 00 00 00 	cmpb   $0x0,0x3041(%rip)        # 405148 <completed.0>
  402107:	75 17                	jne    402120 <__do_global_dtors_aux+0x20>
  402109:	55                   	push   %rbp
  40210a:	48 89 e5             	mov    %rsp,%rbp
  40210d:	e8 7e ff ff ff       	call   402090 <deregister_tm_clones>
  402112:	c6 05 2f 30 00 00 01 	movb   $0x1,0x302f(%rip)        # 405148 <completed.0>
  402119:	5d                   	pop    %rbp
  40211a:	c3                   	ret
  40211b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
  402120:	c3                   	ret
  402121:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  402128:	00 00 00 00 
  40212c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402130 <frame_dummy>:
frame_dummy():
  402130:	eb 8e                	jmp    4020c0 <register_tm_clones>
  402132:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  402139:	00 00 00 
  40213c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402140 <gib_compare_doubles>:
gib_compare_doubles():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1835
    return (*da > *db) - (*da < *db);
  402140:	f2 0f 10 07          	movsd  (%rdi),%xmm0
  402144:	f2 0f 10 0e          	movsd  (%rsi),%xmm1
  402148:	31 c0                	xor    %eax,%eax
  40214a:	66 0f 2f c1          	comisd %xmm1,%xmm0
  40214e:	0f 97 c0             	seta   %al
  402151:	31 d2                	xor    %edx,%edx
  402153:	66 0f 2f c8          	comisd %xmm0,%xmm1
  402157:	0f 97 c2             	seta   %dl
  40215a:	29 d0                	sub    %edx,%eax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1836
}
  40215c:	c3                   	ret
  40215d:	0f 1f 00             	nopl   (%rax)

0000000000402160 <generate_loop_544_808>:
generate_loop_544_808():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:217
{
  402160:	41 56                	push   %r14
  402162:	41 55                	push   %r13
  402164:	41 54                	push   %r12
  402166:	49 89 fc             	mov    %rdi,%r12
  402169:	53                   	push   %rbx
  40216a:	48 83 ec 38          	sub    $0x38,%rsp
  40216e:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:220
    if (fltIf_1031_1126) {
  402173:	48 39 d6             	cmp    %rdx,%rsi
  402176:	75 18                	jne    402190 <generate_loop_544_808+0x30>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:232
}
  402178:	48 83 c4 38          	add    $0x38,%rsp
  40217c:	4c 89 e0             	mov    %r12,%rax
  40217f:	5b                   	pop    %rbx
  402180:	41 5c                	pop    %r12
  402182:	41 5d                	pop    %r13
  402184:	41 5e                	pop    %r14
  402186:	c3                   	ret
  402187:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40218e:	00 00 
  402190:	49 89 d5             	mov    %rdx,%r13
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402193:	48 8b 57 10          	mov    0x10(%rdi),%rdx
  402197:	48 8b 3f             	mov    (%rdi),%rdi
  40219a:	48 89 f3             	mov    %rsi,%rbx
  40219d:	48 01 f7             	add    %rsi,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4021a0:	48 8d 74 24 08       	lea    0x8(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4021a5:	48 0f af fa          	imul   %rdx,%rdi
  4021a9:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4021ae:	e8 8d ef ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:228
                   generate_loop_544_808(vec1_259_938_1130, fltAppE_1033_1131, end_256_936_1124, hd_57_937_1125);
  4021b3:	4c 8b 74 24 08       	mov    0x8(%rsp),%r14
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:226
        GibInt fltAppE_1033_1131 = idx_255_935_1123 + 1;
  4021b8:	48 8d 7b 01          	lea    0x1(%rbx),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:227
        GibVector *tailapp_1221 =
  4021bc:	4c 89 74 24 18       	mov    %r14,0x18(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:220
    if (fltIf_1031_1126) {
  4021c1:	49 39 fd             	cmp    %rdi,%r13
  4021c4:	74 b2                	je     402178 <generate_loop_544_808+0x18>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4021c6:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
  4021cb:	49 03 3c 24          	add    (%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4021cf:	48 8d 74 24 18       	lea    0x18(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4021d4:	48 0f af fa          	imul   %rdx,%rdi
  4021d8:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4021dd:	e8 5e ef ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:226
        GibInt fltAppE_1033_1131 = idx_255_935_1123 + 1;
  4021e2:	48 8d 7b 02          	lea    0x2(%rbx),%rdi
  4021e6:	4c 89 74 24 20       	mov    %r14,0x20(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:220
    if (fltIf_1031_1126) {
  4021eb:	49 39 fd             	cmp    %rdi,%r13
  4021ee:	74 88                	je     402178 <generate_loop_544_808+0x18>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4021f0:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
  4021f5:	49 03 3c 24          	add    (%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4021f9:	48 8d 74 24 20       	lea    0x20(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4021fe:	48 0f af fa          	imul   %rdx,%rdi
  402202:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402207:	e8 34 ef ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:226
        GibInt fltAppE_1033_1131 = idx_255_935_1123 + 1;
  40220c:	48 8d 7b 03          	lea    0x3(%rbx),%rdi
  402210:	4c 89 74 24 28       	mov    %r14,0x28(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:220
    if (fltIf_1031_1126) {
  402215:	49 39 fd             	cmp    %rdi,%r13
  402218:	0f 84 5a ff ff ff    	je     402178 <generate_loop_544_808+0x18>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  40221e:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
  402223:	49 03 3c 24          	add    (%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402227:	48 8d 74 24 28       	lea    0x28(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  40222c:	48 0f af fa          	imul   %rdx,%rdi
  402230:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402235:	e8 06 ef ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:228
                   generate_loop_544_808(vec1_259_938_1130, fltAppE_1033_1131, end_256_936_1124, hd_57_937_1125);
  40223a:	4c 89 e7             	mov    %r12,%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:226
        GibInt fltAppE_1033_1131 = idx_255_935_1123 + 1;
  40223d:	48 8d 73 04          	lea    0x4(%rbx),%rsi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:228
                   generate_loop_544_808(vec1_259_938_1130, fltAppE_1033_1131, end_256_936_1124, hd_57_937_1125);
  402241:	4c 89 f1             	mov    %r14,%rcx
  402244:	4c 89 ea             	mov    %r13,%rdx
  402247:	e8 14 ff ff ff       	call   402160 <generate_loop_544_808>
  40224c:	49 89 c4             	mov    %rax,%r12
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:230
        return tailapp_1221;
  40224f:	e9 24 ff ff ff       	jmp    402178 <generate_loop_544_808+0x18>
  402254:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40225b:	00 00 00 00 
  40225f:	90                   	nop

0000000000402260 <insert_541_809>:
insert_541_809():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:264
{
  402260:	41 55                	push   %r13
  402262:	41 54                	push   %r12
  402264:	55                   	push   %rbp
  402265:	48 89 fd             	mov    %rdi,%rbp
  402268:	48 83 ec 20          	sub    $0x20,%rsp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  40226c:	48 8b 7f 18          	mov    0x18(%rdi),%rdi
  402270:	4c 8b 45 10          	mov    0x10(%rbp),%r8
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:264
  402274:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402279:	48 8b 45 00          	mov    0x0(%rbp),%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:267
    if (fltIf_1038_1147) {
  40227d:	48 85 d2             	test   %rdx,%rdx
  402280:	74 6e                	je     4022f0 <insert_541_809+0x90>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:273
        GibInt i_98_883_996_1152 = n_70_946_1146 - 1;
  402282:	4c 8d 62 ff          	lea    -0x1(%rdx),%r12
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402286:	48 01 c2             	add    %rax,%rdx
  402289:	49 89 f5             	mov    %rsi,%r13
  40228c:	49 0f af d0          	imul   %r8,%rdx
  402290:	49 8d 0c 04          	lea    (%r12,%rax,1),%rcx
  402294:	49 0f af c8          	imul   %r8,%rcx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:278
        GibInt y_72_947_1153 = *tmp_5;
  402298:	48 8b 0c 0f          	mov    (%rdi,%rcx,1),%rcx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  40229c:	48 01 d7             	add    %rdx,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  40229f:	4c 89 c2             	mov    %r8,%rdx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:278
  4022a2:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:282
        if (fltIf_1041_1156) {
  4022a7:	48 39 f1             	cmp    %rsi,%rcx
  4022aa:	7f 1c                	jg     4022c8 <insert_541_809+0x68>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
  4022ac:	48 8d 74 24 08       	lea    0x8(%rsp),%rsi
  4022b1:	e8 8a ee ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:315
}
  4022b6:	48 83 c4 20          	add    $0x20,%rsp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
  4022ba:	48 89 e8             	mov    %rbp,%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:315
  4022bd:	5d                   	pop    %rbp
  4022be:	41 5c                	pop    %r12
  4022c0:	41 5d                	pop    %r13
  4022c2:	c3                   	ret
  4022c3:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
  4022c8:	48 8d 74 24 18       	lea    0x18(%rsp),%rsi
  4022cd:	e8 6e ee ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:304
                       insert_541_809(xs__73_948_1163, x_69_945_1145, fltAppE_1043_1164);
  4022d2:	4c 89 e2             	mov    %r12,%rdx
  4022d5:	4c 89 ee             	mov    %r13,%rsi
  4022d8:	48 89 ef             	mov    %rbp,%rdi
  4022db:	e8 80 ff ff ff       	call   402260 <insert_541_809>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:315
}
  4022e0:	48 83 c4 20          	add    $0x20,%rsp
  4022e4:	5d                   	pop    %rbp
  4022e5:	41 5c                	pop    %r12
  4022e7:	41 5d                	pop    %r13
  4022e9:	c3                   	ret
  4022ea:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4022f0:	49 0f af c0          	imul   %r8,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4022f4:	48 8d 74 24 08       	lea    0x8(%rsp),%rsi
  4022f9:	4c 89 c2             	mov    %r8,%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4022fc:	48 01 c7             	add    %rax,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4022ff:	e8 3c ee ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:315
  402304:	48 83 c4 20          	add    $0x20,%rsp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
  402308:	48 89 e8             	mov    %rbp,%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:315
  40230b:	5d                   	pop    %rbp
  40230c:	41 5c                	pop    %r12
  40230e:	41 5d                	pop    %r13
  402310:	c3                   	ret
  402311:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  402318:	00 00 00 00 
  40231c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402320 <generate_loop_544_803.part.0>:
generate_loop_544_803():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:148
GibVector *generate_loop_544_803(GibVector *vec_254_913_1087,
  402320:	41 56                	push   %r14
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
        GibInt fltPrm_1024_1094 = n_42_916_1090 - idx_255_914_1088;
  402322:	4c 8d 71 ff          	lea    -0x1(%rcx),%r14
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:148
GibVector *generate_loop_544_803(GibVector *vec_254_913_1087,
  402326:	41 55                	push   %r13
  402328:	49 89 d5             	mov    %rdx,%r13
  40232b:	41 54                	push   %r12
  40232d:	49 89 fc             	mov    %rdi,%r12
  402330:	55                   	push   %rbp
  402331:	48 89 cd             	mov    %rcx,%rbp
  402334:	53                   	push   %rbx
  402335:	48 89 f3             	mov    %rsi,%rbx
  402338:	48 83 ec 20          	sub    $0x20,%rsp
generate_loop_544_803.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  40233c:	49 8b 3c 24          	mov    (%r12),%rdi
  402340:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
generate_loop_544_803():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
        GibInt fltPrm_1024_1094 = n_42_916_1090 - idx_255_914_1088;
  402345:	48 89 e8             	mov    %rbp,%rax
generate_loop_544_803.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402348:	48 89 e6             	mov    %rsp,%rsi
generate_loop_544_803():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
  40234b:	48 29 d8             	sub    %rbx,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  40234e:	48 01 df             	add    %rbx,%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
  402351:	48 89 04 24          	mov    %rax,(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402355:	48 0f af fa          	imul   %rdx,%rdi
  402359:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  40235e:	e8 dd ed ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:161
        GibInt fltAppE_1025_1096 = idx_255_914_1088 + 1;
  402363:	48 8d 7b 01          	lea    0x1(%rbx),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:154
    if (fltIf_1023_1091) {
  402367:	49 39 fd             	cmp    %rdi,%r13
  40236a:	74 30                	je     40239c <generate_loop_544_803.part.0+0x7c>
generate_loop_544_803.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  40236c:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
  402371:	49 03 3c 24          	add    (%r12),%rdi
generate_loop_544_803():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
        GibInt fltPrm_1024_1094 = n_42_916_1090 - idx_255_914_1088;
  402375:	4c 89 f0             	mov    %r14,%rax
generate_loop_544_803.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402378:	48 8d 74 24 08       	lea    0x8(%rsp),%rsi
generate_loop_544_803():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
  40237d:	48 29 d8             	sub    %rbx,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402380:	48 0f af fa          	imul   %rdx,%rdi
  402384:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
  402389:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  40238e:	e8 ad ed ff ff       	call   401140 <memcpy@plt>
  402393:	48 8d 7b 02          	lea    0x2(%rbx),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:154
    if (fltIf_1023_1091) {
  402397:	49 39 fd             	cmp    %rdi,%r13
  40239a:	75 14                	jne    4023b0 <generate_loop_544_803.part.0+0x90>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:167
}
  40239c:	48 83 c4 20          	add    $0x20,%rsp
  4023a0:	4c 89 e0             	mov    %r12,%rax
  4023a3:	5b                   	pop    %rbx
  4023a4:	5d                   	pop    %rbp
  4023a5:	41 5c                	pop    %r12
  4023a7:	41 5d                	pop    %r13
  4023a9:	41 5e                	pop    %r14
  4023ab:	c3                   	ret
  4023ac:	0f 1f 40 00          	nopl   0x0(%rax)
generate_loop_544_803.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4023b0:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
  4023b5:	49 03 3c 24          	add    (%r12),%rdi
generate_loop_544_803():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
        GibInt fltPrm_1024_1094 = n_42_916_1090 - idx_255_914_1088;
  4023b9:	48 8d 45 fe          	lea    -0x2(%rbp),%rax
generate_loop_544_803.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4023bd:	48 8d 74 24 10       	lea    0x10(%rsp),%rsi
generate_loop_544_803():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
  4023c2:	48 29 d8             	sub    %rbx,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4023c5:	48 0f af fa          	imul   %rdx,%rdi
  4023c9:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
  4023ce:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4023d3:	e8 68 ed ff ff       	call   401140 <memcpy@plt>
  4023d8:	48 8d 7b 03          	lea    0x3(%rbx),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:154
    if (fltIf_1023_1091) {
  4023dc:	49 39 fd             	cmp    %rdi,%r13
  4023df:	74 bb                	je     40239c <generate_loop_544_803.part.0+0x7c>
generate_loop_544_803.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4023e1:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
  4023e6:	49 03 3c 24          	add    (%r12),%rdi
generate_loop_544_803():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
        GibInt fltPrm_1024_1094 = n_42_916_1090 - idx_255_914_1088;
  4023ea:	48 8d 45 fd          	lea    -0x3(%rbp),%rax
generate_loop_544_803.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4023ee:	48 8d 74 24 18       	lea    0x18(%rsp),%rsi
generate_loop_544_803():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
  4023f3:	48 29 d8             	sub    %rbx,%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:161
        GibInt fltAppE_1025_1096 = idx_255_914_1088 + 1;
  4023f6:	48 83 c3 04          	add    $0x4,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4023fa:	48 0f af fa          	imul   %rdx,%rdi
  4023fe:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
        GibInt fltPrm_1024_1094 = n_42_916_1090 - idx_255_914_1088;
  402403:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402408:	e8 33 ed ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:154
    if (fltIf_1023_1091) {
  40240d:	49 39 dd             	cmp    %rbx,%r13
  402410:	74 8a                	je     40239c <generate_loop_544_803.part.0+0x7c>
  402412:	e9 25 ff ff ff       	jmp    40233c <generate_loop_544_803.part.0+0x1c>
generate_loop_544_803.part.0():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:154
  402417:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40241e:	00 00 

0000000000402420 <generate_loop_544_804.part.0>:
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:168
GibVector *generate_loop_544_804(GibVector *vec_254_918_1097,
  402420:	41 55                	push   %r13
  402422:	49 89 d5             	mov    %rdx,%r13
  402425:	41 54                	push   %r12
  402427:	49 89 fc             	mov    %rdi,%r12
  40242a:	55                   	push   %rbp
  40242b:	48 89 f5             	mov    %rsi,%rbp
  40242e:	53                   	push   %rbx
  40242f:	48 89 cb             	mov    %rcx,%rbx
  402432:	48 83 ec 28          	sub    $0x28,%rsp
generate_loop_544_804.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402436:	48 8b 03             	mov    (%rbx),%rax
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
        GibInt fltPrm_1027_1104 = *tmp_1;
  402439:	48 8b 53 18          	mov    0x18(%rbx),%rdx
generate_loop_544_804.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  40243d:	48 89 e6             	mov    %rsp,%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402440:	49 8b 3c 24          	mov    (%r12),%rdi
  402444:	48 01 e8             	add    %rbp,%rax
  402447:	48 0f af 43 10       	imul   0x10(%rbx),%rax
  40244c:	48 01 ef             	add    %rbp,%rdi
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
  40244f:	48 8b 04 02          	mov    (%rdx,%rax,1),%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402453:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
  402458:	48 0f af fa          	imul   %rdx,%rdi
  40245c:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
  402461:	48 89 04 24          	mov    %rax,(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402465:	e8 d6 ec ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:186
        GibInt fltAppE_1028_1106 = idx_255_919_1098 + 1;
  40246a:	48 8d 7d 01          	lea    0x1(%rbp),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:175
    if (fltIf_1026_1101) {
  40246e:	49 39 fd             	cmp    %rdi,%r13
  402471:	74 3d                	je     4024b0 <generate_loop_544_804.part.0+0x90>
generate_loop_544_804.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402473:	48 8b 03             	mov    (%rbx),%rax
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
        GibInt fltPrm_1027_1104 = *tmp_1;
  402476:	48 8b 53 18          	mov    0x18(%rbx),%rdx
generate_loop_544_804.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  40247a:	48 8d 74 24 08       	lea    0x8(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  40247f:	48 01 f8             	add    %rdi,%rax
  402482:	48 0f af 43 10       	imul   0x10(%rbx),%rax
  402487:	49 03 3c 24          	add    (%r12),%rdi
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
  40248b:	48 8b 04 02          	mov    (%rdx,%rax,1),%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  40248f:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
  402494:	48 0f af fa          	imul   %rdx,%rdi
  402498:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
  40249d:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4024a2:	e8 99 ec ff ff       	call   401140 <memcpy@plt>
  4024a7:	48 8d 7d 02          	lea    0x2(%rbp),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:175
    if (fltIf_1026_1101) {
  4024ab:	49 39 fd             	cmp    %rdi,%r13
  4024ae:	75 10                	jne    4024c0 <generate_loop_544_804.part.0+0xa0>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:192
}
  4024b0:	48 83 c4 28          	add    $0x28,%rsp
  4024b4:	4c 89 e0             	mov    %r12,%rax
  4024b7:	5b                   	pop    %rbx
  4024b8:	5d                   	pop    %rbp
  4024b9:	41 5c                	pop    %r12
  4024bb:	41 5d                	pop    %r13
  4024bd:	c3                   	ret
  4024be:	66 90                	xchg   %ax,%ax
generate_loop_544_804.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4024c0:	48 8b 03             	mov    (%rbx),%rax
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
        GibInt fltPrm_1027_1104 = *tmp_1;
  4024c3:	48 8b 53 18          	mov    0x18(%rbx),%rdx
generate_loop_544_804.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4024c7:	48 8d 74 24 10       	lea    0x10(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4024cc:	48 01 f8             	add    %rdi,%rax
  4024cf:	48 0f af 43 10       	imul   0x10(%rbx),%rax
  4024d4:	49 03 3c 24          	add    (%r12),%rdi
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
  4024d8:	48 8b 04 02          	mov    (%rdx,%rax,1),%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4024dc:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
  4024e1:	48 0f af fa          	imul   %rdx,%rdi
  4024e5:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
  4024ea:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4024ef:	e8 4c ec ff ff       	call   401140 <memcpy@plt>
  4024f4:	48 8d 7d 03          	lea    0x3(%rbp),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:175
    if (fltIf_1026_1101) {
  4024f8:	49 39 fd             	cmp    %rdi,%r13
  4024fb:	74 b3                	je     4024b0 <generate_loop_544_804.part.0+0x90>
generate_loop_544_804.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4024fd:	48 8b 03             	mov    (%rbx),%rax
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
        GibInt fltPrm_1027_1104 = *tmp_1;
  402500:	48 8b 53 18          	mov    0x18(%rbx),%rdx
generate_loop_544_804.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402504:	48 8d 74 24 18       	lea    0x18(%rsp),%rsi
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:186
        GibInt fltAppE_1028_1106 = idx_255_919_1098 + 1;
  402509:	48 83 c5 04          	add    $0x4,%rbp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  40250d:	48 01 f8             	add    %rdi,%rax
  402510:	48 0f af 43 10       	imul   0x10(%rbx),%rax
generate_loop_544_804.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402515:	49 03 3c 24          	add    (%r12),%rdi
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
        GibInt fltPrm_1027_1104 = *tmp_1;
  402519:	48 8b 04 02          	mov    (%rdx,%rax,1),%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  40251d:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
  402522:	48 0f af fa          	imul   %rdx,%rdi
  402526:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
  40252b:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402530:	e8 0b ec ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:175
    if (fltIf_1026_1101) {
  402535:	49 39 ed             	cmp    %rbp,%r13
  402538:	0f 84 72 ff ff ff    	je     4024b0 <generate_loop_544_804.part.0+0x90>
  40253e:	e9 f3 fe ff ff       	jmp    402436 <generate_loop_544_804.part.0+0x16>
generate_loop_544_804.part.0():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:175
  402543:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40254a:	00 00 00 00 
  40254e:	66 90                	xchg   %ax,%ax

0000000000402550 <isort_540_807.part.0>:
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:233
GibVector *isort_540_807(GibVector *xs_61_939_1132, GibVector *b_62_940_1133,
  402550:	41 57                	push   %r15
  402552:	41 56                	push   %r14
  402554:	41 55                	push   %r13
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:247
            GibInt fltAppE_1036_1139 = n_63_941_1134 - 1;
  402556:	4c 8d 6a ff          	lea    -0x1(%rdx),%r13
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:233
GibVector *isort_540_807(GibVector *xs_61_939_1132, GibVector *b_62_940_1133,
  40255a:	41 54                	push   %r12
  40255c:	49 89 d4             	mov    %rdx,%r12
  40255f:	53                   	push   %rbx
  402560:	48 89 fb             	mov    %rdi,%rbx
  402563:	48 83 ec 10          	sub    $0x10,%rsp
  402567:	48 8b 17             	mov    (%rdi),%rdx
isort_540_807.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:533
    return (vec->upper - vec->lower);
  40256a:	48 8b 47 08          	mov    0x8(%rdi),%rax
  40256e:	48 29 d0             	sub    %rdx,%rax
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:239
    if (fltIf_1034_1137) {
  402571:	48 83 f8 01          	cmp    $0x1,%rax
  402575:	0f 8e 05 01 00 00    	jle    402680 <isort_540_807.part.0+0x130>
  40257b:	49 89 f6             	mov    %rsi,%r14
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:244
        if (fltIf_1035_1138) {
  40257e:	4d 85 ed             	test   %r13,%r13
  402581:	75 6d                	jne    4025f0 <isort_540_807.part.0+0xa0>
isort_540_807.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402583:	48 83 c2 01          	add    $0x1,%rdx
  402587:	48 0f af 57 10       	imul   0x10(%rdi),%rdx
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:254
            GibInt fltAppE_1037_1143 = *tmp_4;
  40258c:	48 8b 47 18          	mov    0x18(%rdi),%rax
isort_540_807.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402590:	48 8b 4e 18          	mov    0x18(%rsi),%rcx
  402594:	48 8b 3e             	mov    (%rsi),%rdi
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:254
  402597:	4c 8b 3c 10          	mov    (%rax,%rdx,1),%r15
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  40259b:	48 8b 56 10          	mov    0x10(%rsi),%rdx
  40259f:	4c 89 3c 24          	mov    %r15,(%rsp)
  4025a3:	49 8d 44 3d 00       	lea    0x0(%r13,%rdi,1),%rax
isort_540_807.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4025a8:	4c 01 e7             	add    %r12,%rdi
  4025ab:	48 0f af c2          	imul   %rdx,%rax
  4025af:	48 0f af fa          	imul   %rdx,%rdi
insert_541_809():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:278
        GibInt y_72_947_1153 = *tmp_5;
  4025b3:	48 8b 04 01          	mov    (%rcx,%rax,1),%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4025b7:	48 01 cf             	add    %rcx,%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:278
  4025ba:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:282
        if (fltIf_1041_1156) {
  4025bf:	4c 39 f8             	cmp    %r15,%rax
  4025c2:	0f 8e a8 00 00 00    	jle    402670 <isort_540_807.part.0+0x120>
isort_540_807.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4025c8:	48 8d 74 24 08       	lea    0x8(%rsp),%rsi
  4025cd:	e8 6e eb ff ff       	call   401140 <memcpy@plt>
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:261
}
  4025d2:	48 83 c4 10          	add    $0x10,%rsp
insert_541_809():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:304
                       insert_541_809(xs__73_948_1163, x_69_945_1145, fltAppE_1043_1164);
  4025d6:	4c 89 ea             	mov    %r13,%rdx
  4025d9:	4c 89 fe             	mov    %r15,%rsi
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:261
}
  4025dc:	5b                   	pop    %rbx
insert_541_809():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:304
                       insert_541_809(xs__73_948_1163, x_69_945_1145, fltAppE_1043_1164);
  4025dd:	4c 89 f7             	mov    %r14,%rdi
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:261
}
  4025e0:	41 5c                	pop    %r12
  4025e2:	41 5d                	pop    %r13
  4025e4:	41 5e                	pop    %r14
  4025e6:	41 5f                	pop    %r15
insert_541_809():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:304
                       insert_541_809(xs__73_948_1163, x_69_945_1145, fltAppE_1043_1164);
  4025e8:	e9 73 fc ff ff       	jmp    402260 <insert_541_809>
  4025ed:	0f 1f 00             	nopl   (%rax)
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:244
        if (fltIf_1035_1138) {
  4025f0:	4d 89 e0             	mov    %r12,%r8
  4025f3:	49 83 e8 02          	sub    $0x2,%r8
  4025f7:	0f 85 93 00 00 00    	jne    402690 <isort_540_807.part.0+0x140>
isort_540_807.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4025fd:	4c 01 ea             	add    %r13,%rdx
  402600:	48 0f af 53 10       	imul   0x10(%rbx),%rdx
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:254
            GibInt fltAppE_1037_1143 = *tmp_4;
  402605:	48 8b 43 18          	mov    0x18(%rbx),%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:256
                       insert_541_809(xs__66_943_1140, fltAppE_1037_1143, n_63_941_1134);
  402609:	4c 89 f7             	mov    %r14,%rdi
  40260c:	48 8b 34 10          	mov    (%rax,%rdx,1),%rsi
  402610:	4c 89 ea             	mov    %r13,%rdx
  402613:	e8 48 fc ff ff       	call   402260 <insert_541_809>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402618:	48 8b 13             	mov    (%rbx),%rdx
isort_540_807.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  40261b:	48 8b 38             	mov    (%rax),%rdi
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:256
  40261e:	49 89 c6             	mov    %rax,%r14
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402621:	4c 01 e2             	add    %r12,%rdx
  402624:	48 0f af 53 10       	imul   0x10(%rbx),%rdx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:254
            GibInt fltAppE_1037_1143 = *tmp_4;
  402629:	48 8b 43 18          	mov    0x18(%rbx),%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  40262d:	49 8b 4e 18          	mov    0x18(%r14),%rcx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:254
  402631:	4c 8b 3c 10          	mov    (%rax,%rdx,1),%r15
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402635:	49 8b 56 10          	mov    0x10(%r14),%rdx
  402639:	4c 89 3c 24          	mov    %r15,(%rsp)
insert_541_809():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:267
    if (fltIf_1038_1147) {
  40263d:	4d 85 e4             	test   %r12,%r12
  402640:	0f 85 5d ff ff ff    	jne    4025a3 <isort_540_807.part.0+0x53>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402646:	48 0f af fa          	imul   %rdx,%rdi
isort_540_807.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  40264a:	48 89 e6             	mov    %rsp,%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  40264d:	48 01 cf             	add    %rcx,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402650:	e8 eb ea ff ff       	call   401140 <memcpy@plt>
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:261
}
  402655:	48 83 c4 10          	add    $0x10,%rsp
  402659:	4c 89 f0             	mov    %r14,%rax
  40265c:	5b                   	pop    %rbx
  40265d:	41 5c                	pop    %r12
  40265f:	41 5d                	pop    %r13
  402661:	41 5e                	pop    %r14
  402663:	41 5f                	pop    %r15
  402665:	c3                   	ret
  402666:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40266d:	00 00 00 
isort_540_807.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
  402670:	48 89 e6             	mov    %rsp,%rsi
  402673:	e8 c8 ea ff ff       	call   401140 <memcpy@plt>
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:258
            return tailapp_1222;
  402678:	eb db                	jmp    402655 <isort_540_807.part.0+0x105>
  40267a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:239
    if (fltIf_1034_1137) {
  402680:	48 89 d7             	mov    %rdx,%rdi
  402683:	49 89 de             	mov    %rbx,%r14
  402686:	eb 99                	jmp    402621 <isort_540_807.part.0+0xd1>
  402688:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40268f:	00 
  402690:	4c 89 c2             	mov    %r8,%rdx
  402693:	e8 b8 fe ff ff       	call   402550 <isort_540_807.part.0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402698:	48 8b 13             	mov    (%rbx),%rdx
  40269b:	49 89 c6             	mov    %rax,%r14
  40269e:	e9 5a ff ff ff       	jmp    4025fd <isort_540_807.part.0+0xad>
isort_540_807.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4026a3:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  4026aa:	00 00 00 00 
  4026ae:	66 90                	xchg   %ax,%ax

00000000004026b0 <shift_543_811.part.0>:
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:348
GibVector *shift_543_811(GibInt j_74_955_1180, GibVector *ys_76_956_1181)
  4026b0:	41 54                	push   %r12
  4026b2:	49 89 f4             	mov    %rsi,%r12
  4026b5:	53                   	push   %rbx
  4026b6:	48 89 fb             	mov    %rdi,%rbx
  4026b9:	48 83 ec 18          	sub    $0x18,%rsp
shift_543_811.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4026bd:	49 8b 04 24          	mov    (%r12),%rax
  4026c1:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
  4026c6:	49 8b 74 24 18       	mov    0x18(%r12),%rsi
  4026cb:	48 8d 3c 03          	lea    (%rbx,%rax,1),%rdi
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:360
        GibInt i_98_883_1010_1187 = j_74_955_1180 - 1;
  4026cf:	48 83 eb 01          	sub    $0x1,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4026d3:	48 0f af fa          	imul   %rdx,%rdi
shift_543_811.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4026d7:	48 01 d8             	add    %rbx,%rax
  4026da:	48 0f af c2          	imul   %rdx,%rax
  4026de:	48 01 f7             	add    %rsi,%rdi
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:359
        GibInt a_78_957_1185 = *tmp_8;
  4026e1:	48 8b 0f             	mov    (%rdi),%rcx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:365
        GibInt b_79_958_1188 = *tmp_7;
  4026e4:	48 8b 04 06          	mov    (%rsi,%rax,1),%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:359
        GibInt a_78_957_1185 = *tmp_8;
  4026e8:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:365
        GibInt b_79_958_1188 = *tmp_7;
  4026ed:	48 89 04 24          	mov    %rax,(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:376
            if (fltIf_1052_1192) {
  4026f1:	48 39 c1             	cmp    %rax,%rcx
  4026f4:	7f 2c                	jg     402722 <shift_543_811.part.0+0x72>
shift_543_811.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4026f6:	48 89 e6             	mov    %rsp,%rsi
  4026f9:	e8 42 ea ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4026fe:	49 8b 3c 24          	mov    (%r12),%rdi
  402702:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402707:	48 8d 74 24 08       	lea    0x8(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  40270c:	48 01 df             	add    %rbx,%rdi
  40270f:	48 0f af fa          	imul   %rdx,%rdi
  402713:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402718:	e8 23 ea ff ff       	call   401140 <memcpy@plt>
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:352
    if (fltIf_1048_1182) {
  40271d:	48 85 db             	test   %rbx,%rbx
  402720:	75 9b                	jne    4026bd <shift_543_811.part.0+0xd>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:403
}
  402722:	48 83 c4 18          	add    $0x18,%rsp
  402726:	4c 89 e0             	mov    %r12,%rax
  402729:	5b                   	pop    %rbx
  40272a:	41 5c                	pop    %r12
  40272c:	c3                   	ret
shift_543_811.part.0():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:403
  40272d:	0f 1f 00             	nopl   (%rax)

0000000000402730 <gib_vector_alloc.constprop.0>:
gib_vector_alloc.constprop.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:512
GibVector *gib_vector_alloc(GibInt num, size_t elt_size)
  402730:	41 54                	push   %r12
  402732:	53                   	push   %rbx
  402733:	48 89 fb             	mov    %rdi,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  402736:	bf 20 00 00 00       	mov    $0x20,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:512
GibVector *gib_vector_alloc(GibInt num, size_t elt_size)
  40273b:	48 83 ec 08          	sub    $0x8,%rsp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  40273f:	e8 1c ea ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:515
    if (vec == NULL) {
  402744:	48 85 c0             	test   %rax,%rax
  402747:	74 3b                	je     402784 <gib_vector_alloc.constprop.0+0x54>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:519
    void *data = (void *) gib_alloc(num * elt_size);
  402749:	48 8d 3c dd 00 00 00 	lea    0x0(,%rbx,8),%rdi
  402750:	00 
  402751:	49 89 c4             	mov    %rax,%r12
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  402754:	e8 07 ea ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:520
    if (data == NULL) {
  402759:	48 85 c0             	test   %rax,%rax
  40275c:	74 48                	je     4027a6 <gib_vector_alloc.constprop.0+0x76>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:527
    vec->data = data;
  40275e:	49 89 44 24 18       	mov    %rax,0x18(%r12)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:529
}
  402763:	4c 89 e0             	mov    %r12,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:524
    vec->lower = 0;
  402766:	49 c7 04 24 00 00 00 	movq   $0x0,(%r12)
  40276d:	00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:525
    vec->upper = num;
  40276e:	49 89 5c 24 08       	mov    %rbx,0x8(%r12)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:526
    vec->elt_size = elt_size;
  402773:	49 c7 44 24 10 08 00 	movq   $0x8,0x10(%r12)
  40277a:	00 00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:529
}
  40277c:	48 83 c4 08          	add    $0x8,%rsp
  402780:	5b                   	pop    %rbx
  402781:	41 5c                	pop    %r12
  402783:	c3                   	ret
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:516
        fprintf(stderr, "alloc_vector: gib_alloc failed: %zu", sizeof(GibVector));
  402784:	48 8b 3d b5 29 00 00 	mov    0x29b5(%rip),%rdi        # 405140 <stderr@GLIBC_2.2.5>
  40278b:	ba 20 00 00 00       	mov    $0x20,%edx
  402790:	be 08 30 40 00       	mov    $0x403008,%esi
  402795:	31 c0                	xor    %eax,%eax
  402797:	e8 94 e9 ff ff       	call   401130 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:517
        exit(1);
  40279c:	bf 01 00 00 00       	mov    $0x1,%edi
  4027a1:	e8 0a ea ff ff       	call   4011b0 <exit@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:521
        fprintf(stderr, "alloc_vector: gib_alloc failed: %zu", sizeof(num * elt_size));
  4027a6:	48 8b 3d 93 29 00 00 	mov    0x2993(%rip),%rdi        # 405140 <stderr@GLIBC_2.2.5>
  4027ad:	ba 08 00 00 00       	mov    $0x8,%edx
  4027b2:	be 08 30 40 00       	mov    $0x403008,%esi
  4027b7:	e8 74 e9 ff ff       	call   401130 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:522
        exit(1);
  4027bc:	bf 01 00 00 00       	mov    $0x1,%edi
  4027c1:	e8 ea e9 ff ff       	call   4011b0 <exit@plt>
  4027c6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4027cd:	00 00 00 

00000000004027d0 <gib_shadowstack_initialize.part.0.constprop.0>:
gib_shadowstack_initialize.part.0.constprop.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1483
static void gib_shadowstack_initialize(GibShadowstack* stack, size_t stack_size)
  4027d0:	48 83 ec 08          	sub    $0x8,%rsp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1487
        fprintf(stderr, "gib_shadowstack_initialize: gib_alloc failed: %zu",
  4027d4:	ba 00 00 00 06       	mov    $0x6000000,%edx
  4027d9:	be 30 30 40 00       	mov    $0x403030,%esi
  4027de:	31 c0                	xor    %eax,%eax
  4027e0:	48 8b 3d 59 29 00 00 	mov    0x2959(%rip),%rdi        # 405140 <stderr@GLIBC_2.2.5>
  4027e7:	e8 44 e9 ff ff       	call   401130 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1489
        exit(1);
  4027ec:	bf 01 00 00 00       	mov    $0x1,%edi
  4027f1:	e8 ba e9 ff ff       	call   4011b0 <exit@plt>
  4027f6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4027fd:	00 00 00 

0000000000402800 <gib_print_timing_array>:
gib_print_timing_array():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:673
void gib_print_timing_array(GibVector *times) {
  402800:	41 55                	push   %r13
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:674
    printf("ITER TIMES: [");
  402802:	31 c0                	xor    %eax,%eax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:673
void gib_print_timing_array(GibVector *times) {
  402804:	49 89 fd             	mov    %rdi,%r13
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:674
    printf("ITER TIMES: [");
  402807:	bf 03 37 40 00       	mov    $0x403703,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:673
void gib_print_timing_array(GibVector *times) {
  40280c:	41 54                	push   %r12
  40280e:	55                   	push   %rbp
  40280f:	53                   	push   %rbx
  402810:	48 83 ec 08          	sub    $0x8,%rsp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:674
    printf("ITER TIMES: [");
  402814:	e8 a7 e8 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:533
    return (vec->upper - vec->lower);
  402819:	49 8b 45 00          	mov    0x0(%r13),%rax
  40281d:	49 8b 6d 08          	mov    0x8(%r13),%rbp
  402821:	48 29 c5             	sub    %rax,%rbp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:677
    for(GibInt i = 0; i < n; i++) {
  402824:	48 85 ed             	test   %rbp,%rbp
  402827:	7e 59                	jle    402882 <gib_print_timing_array+0x82>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:679
        if (i == (n-1)) {
  402829:	4c 8d 65 ff          	lea    -0x1(%rbp),%r12
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:677
    for(GibInt i = 0; i < n; i++) {
  40282d:	31 db                	xor    %ebx,%ebx
  40282f:	eb 23                	jmp    402854 <gib_print_timing_array+0x54>
  402831:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:683
            printf("%f, ",*d);
  402838:	bf 14 37 40 00       	mov    $0x403714,%edi
  40283d:	b8 01 00 00 00       	mov    $0x1,%eax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:677
    for(GibInt i = 0; i < n; i++) {
  402842:	48 83 c3 01          	add    $0x1,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:683
            printf("%f, ",*d);
  402846:	e8 75 e8 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:677
    for(GibInt i = 0; i < n; i++) {
  40284b:	48 39 eb             	cmp    %rbp,%rbx
  40284e:	74 32                	je     402882 <gib_print_timing_array+0x82>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402850:	49 8b 45 00          	mov    0x0(%r13),%rax
  402854:	48 01 d8             	add    %rbx,%rax
  402857:	49 0f af 45 10       	imul   0x10(%r13),%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:680
            printf("%f",*d);
  40285c:	49 8b 55 18          	mov    0x18(%r13),%rdx
  402860:	f2 0f 10 04 02       	movsd  (%rdx,%rax,1),%xmm0
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:679
        if (i == (n-1)) {
  402865:	4c 39 e3             	cmp    %r12,%rbx
  402868:	75 ce                	jne    402838 <gib_print_timing_array+0x38>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:680
            printf("%f",*d);
  40286a:	bf 11 37 40 00       	mov    $0x403711,%edi
  40286f:	b8 01 00 00 00       	mov    $0x1,%eax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:677
    for(GibInt i = 0; i < n; i++) {
  402874:	48 83 c3 01          	add    $0x1,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:680
            printf("%f",*d);
  402878:	e8 43 e8 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:677
    for(GibInt i = 0; i < n; i++) {
  40287d:	48 39 dd             	cmp    %rbx,%rbp
  402880:	75 ce                	jne    402850 <gib_print_timing_array+0x50>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:688
}
  402882:	48 83 c4 08          	add    $0x8,%rsp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:686
    printf("]\n");
  402886:	bf 19 37 40 00       	mov    $0x403719,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:688
}
  40288b:	5b                   	pop    %rbx
  40288c:	5d                   	pop    %rbp
  40288d:	41 5c                	pop    %r12
  40288f:	41 5d                	pop    %r13
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:686
    printf("]\n");
  402891:	e9 ea e7 ff ff       	jmp    401080 <puts@plt>
  402896:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40289d:	00 00 00 

00000000004028a0 <gib_show_usage>:
gib_show_usage():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1797
{
  4028a0:	53                   	push   %rbx
  4028a1:	48 89 fb             	mov    %rdi,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1798
    printf("\n");
  4028a4:	bf 0a 00 00 00       	mov    $0xa,%edi
  4028a9:	e8 92 e7 ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1799
    printf("This binary was generated by the Gibbon compiler.\n");
  4028ae:	bf 68 30 40 00       	mov    $0x403068,%edi
  4028b3:	e8 c8 e7 ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1800
    printf("\n");
  4028b8:	bf 0a 00 00 00       	mov    $0xa,%edi
  4028bd:	e8 7e e7 ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1801
    printf("Usage: %s [OPTIONS...]\n", argv[0]);
  4028c2:	48 8b 33             	mov    (%rbx),%rsi
  4028c5:	bf 1b 37 40 00       	mov    $0x40371b,%edi
  4028ca:	31 c0                	xor    %eax,%eax
  4028cc:	e8 ef e7 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1803
    printf("\n");
  4028d1:	bf 0a 00 00 00       	mov    $0xa,%edi
  4028d6:	e8 65 e7 ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1804
    printf("Options:\n");
  4028db:	bf 33 37 40 00       	mov    $0x403733,%edi
  4028e0:	e8 9b e7 ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1805
    printf(" --biginf-buffer-size <bytes>   Set the buffer size (default %" PRId64 ").\n", gib_global_biginf_init_chunk_size);
  4028e5:	48 8b 35 04 28 00 00 	mov    0x2804(%rip),%rsi        # 4050f0 <gib_global_biginf_init_chunk_size>
  4028ec:	bf a0 30 40 00       	mov    $0x4030a0,%edi
  4028f1:	31 c0                	xor    %eax,%eax
  4028f3:	e8 c8 e7 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1806
    printf(" --inf-buffer-size <bytes>      Set the buffer size (default %" PRId64 ").\n", gib_global_inf_init_chunk_size);
  4028f8:	48 8b 35 e9 27 00 00 	mov    0x27e9(%rip),%rsi        # 4050e8 <gib_global_inf_init_chunk_size>
  4028ff:	bf e8 30 40 00       	mov    $0x4030e8,%edi
  402904:	31 c0                	xor    %eax,%eax
  402906:	e8 b5 e7 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1807
    printf(" --bench-input <path>           Set the input file read for benchmarking. Applies only\n");
  40290b:	bf 30 31 40 00       	mov    $0x403130,%edi
  402910:	e8 6b e7 ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1808
    printf("                                If the program was *compiled* with --bench-fun. \n");
  402915:	bf 88 31 40 00       	mov    $0x403188,%edi
  40291a:	e8 61 e7 ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1809
    printf("\n");
  40291f:	bf 0a 00 00 00       	mov    $0xa,%edi
  402924:	e8 17 e7 ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1810
    printf(" --array-input <path>           Set the file from which to read the array input.\n");
  402929:	bf e0 31 40 00       	mov    $0x4031e0,%edi
  40292e:	e8 4d e7 ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1811
    printf(" --array-input-length <int>     Set the size of the array input file.\n");
  402933:	bf 38 32 40 00       	mov    $0x403238,%edi
  402938:	e8 43 e7 ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1812
    printf(" --iterate <int>                Set the number of timing iterations to perform (default 1).\n");
  40293d:	bf 80 32 40 00       	mov    $0x403280,%edi
  402942:	e8 39 e7 ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1814
    printf(" --size-param <int>             A parameter for size available as a language primitive which allows user to specify the size at runtime (default 1).\n");
  402947:	bf e0 32 40 00       	mov    $0x4032e0,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1816
}
  40294c:	5b                   	pop    %rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1814
    printf(" --size-param <int>             A parameter for size available as a language primitive which allows user to specify the size at runtime (default 1).\n");
  40294d:	e9 2e e7 ff ff       	jmp    401080 <puts@plt>
  402952:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  402959:	00 00 00 00 
  40295d:	0f 1f 00             	nopl   (%rax)

0000000000402960 <check_args.part.0>:
check_args.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1876
void check_args(int i, int argc, char **argv, char *parameter){
  402960:	55                   	push   %rbp
  402961:	48 89 fd             	mov    %rdi,%rbp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1878
        fprintf(stderr, "Not enough arguments after %s, expected <int>.\n", parameter);
  402964:	48 8b 3d d5 27 00 00 	mov    0x27d5(%rip),%rdi        # 405140 <stderr@GLIBC_2.2.5>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1876
void check_args(int i, int argc, char **argv, char *parameter){
  40296b:	48 89 f2             	mov    %rsi,%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1878
        fprintf(stderr, "Not enough arguments after %s, expected <int>.\n", parameter);
  40296e:	31 c0                	xor    %eax,%eax
  402970:	be 78 33 40 00       	mov    $0x403378,%esi
  402975:	e8 b6 e7 ff ff       	call   401130 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1879
        gib_show_usage(argv);
  40297a:	48 89 ef             	mov    %rbp,%rdi
  40297d:	e8 1e ff ff ff       	call   4028a0 <gib_show_usage>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1880
        exit(1);
  402982:	bf 01 00 00 00       	mov    $0x1,%edi
  402987:	e8 24 e8 ff ff       	call   4011b0 <exit@plt>
  40298c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402990 <gib_add_symbol>:
gib_add_symbol():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:411
{
  402990:	41 56                	push   %r14
  402992:	41 55                	push   %r13
  402994:	41 54                	push   %r12
  402996:	49 89 fc             	mov    %rdi,%r12
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  402999:	bf 40 01 00 00       	mov    $0x140,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:411
{
  40299e:	55                   	push   %rbp
  40299f:	48 89 f5             	mov    %rsi,%rbp
  4029a2:	53                   	push   %rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4029a3:	e8 b8 e7 ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:415
    strcpy(s->value, value);
  4029a8:	48 89 ee             	mov    %rbp,%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:414
    s->idx = idx;
  4029ab:	4c 89 20             	mov    %r12,(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:415
    strcpy(s->value, value);
  4029ae:	48 8d 78 08          	lea    0x8(%rax),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4029b2:	48 89 c3             	mov    %rax,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:415
    strcpy(s->value, value);
  4029b5:	e8 a6 e6 ff ff       	call   401060 <strcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416
    HASH_ADD(hh, global_sym_table, idx, sizeof(GibSym), s);
  4029ba:	4c 89 e0             	mov    %r12,%rax
  4029bd:	4c 89 e1             	mov    %r12,%rcx
  4029c0:	48 89 9b 30 01 00 00 	mov    %rbx,0x130(%rbx)
  4029c7:	48 c1 e8 38          	shr    $0x38,%rax
  4029cb:	48 c1 e9 30          	shr    $0x30,%rcx
  4029cf:	c7 83 38 01 00 00 08 	movl   $0x8,0x138(%rbx)
  4029d6:	00 00 00 
  4029d9:	48 89 c2             	mov    %rax,%rdx
  4029dc:	4c 89 e0             	mov    %r12,%rax
  4029df:	0f b6 c9             	movzbl %cl,%ecx
  4029e2:	48 c1 e8 20          	shr    $0x20,%rax
  4029e6:	c1 e2 18             	shl    $0x18,%edx
  4029e9:	0f b6 c0             	movzbl %al,%eax
  4029ec:	c1 e1 10             	shl    $0x10,%ecx
  4029ef:	8d 94 02 b9 79 37 9e 	lea    -0x61c88647(%rdx,%rax,1),%edx
  4029f6:	8d 04 0a             	lea    (%rdx,%rcx,1),%eax
  4029f9:	4c 89 e1             	mov    %r12,%rcx
  4029fc:	44 89 e2             	mov    %r12d,%edx
  4029ff:	48 c1 e9 28          	shr    $0x28,%rcx
  402a03:	81 e2 ff ff 00 ff    	and    $0xff00ffff,%edx
  402a09:	0f b6 c9             	movzbl %cl,%ecx
  402a0c:	c1 e1 08             	shl    $0x8,%ecx
  402a0f:	01 c1                	add    %eax,%ecx
  402a11:	4c 89 e0             	mov    %r12,%rax
  402a14:	48 c1 e8 10          	shr    $0x10,%rax
  402a18:	0f b6 c0             	movzbl %al,%eax
  402a1b:	c1 e0 10             	shl    $0x10,%eax
  402a1e:	8d 84 02 c2 ba 49 9f 	lea    -0x60b6453e(%rdx,%rax,1),%eax
  402a25:	29 c8                	sub    %ecx,%eax
  402a27:	81 c1 09 41 12 01    	add    $0x1124109,%ecx
  402a2d:	35 6d f7 07 00       	xor    $0x7f76d,%eax
  402a32:	29 c1                	sub    %eax,%ecx
  402a34:	89 ca                	mov    %ecx,%edx
  402a36:	89 c1                	mov    %eax,%ecx
  402a38:	c1 e1 08             	shl    $0x8,%ecx
  402a3b:	31 d1                	xor    %edx,%ecx
  402a3d:	ba f7 be ed fe       	mov    $0xfeedbef7,%edx
  402a42:	29 c2                	sub    %eax,%edx
  402a44:	29 c8                	sub    %ecx,%eax
  402a46:	89 d6                	mov    %edx,%esi
  402a48:	89 ca                	mov    %ecx,%edx
  402a4a:	29 ce                	sub    %ecx,%esi
  402a4c:	c1 ea 0d             	shr    $0xd,%edx
  402a4f:	31 f2                	xor    %esi,%edx
  402a51:	89 d6                	mov    %edx,%esi
  402a53:	29 d0                	sub    %edx,%eax
  402a55:	29 d1                	sub    %edx,%ecx
  402a57:	c1 ee 0c             	shr    $0xc,%esi
  402a5a:	31 f0                	xor    %esi,%eax
  402a5c:	89 ce                	mov    %ecx,%esi
  402a5e:	89 c1                	mov    %eax,%ecx
  402a60:	29 c6                	sub    %eax,%esi
  402a62:	29 c2                	sub    %eax,%edx
  402a64:	c1 e1 10             	shl    $0x10,%ecx
  402a67:	31 f1                	xor    %esi,%ecx
  402a69:	89 d6                	mov    %edx,%esi
  402a6b:	89 ca                	mov    %ecx,%edx
  402a6d:	29 ce                	sub    %ecx,%esi
  402a6f:	29 c8                	sub    %ecx,%eax
  402a71:	c1 ea 05             	shr    $0x5,%edx
  402a74:	31 f2                	xor    %esi,%edx
  402a76:	89 d6                	mov    %edx,%esi
  402a78:	29 d0                	sub    %edx,%eax
  402a7a:	29 d1                	sub    %edx,%ecx
  402a7c:	c1 ee 03             	shr    $0x3,%esi
  402a7f:	31 f0                	xor    %esi,%eax
  402a81:	89 c5                	mov    %eax,%ebp
  402a83:	29 c1                	sub    %eax,%ecx
  402a85:	29 c2                	sub    %eax,%edx
  402a87:	48 8b 05 ca 26 00 00 	mov    0x26ca(%rip),%rax        # 405158 <global_sym_table>
  402a8e:	c1 e5 0a             	shl    $0xa,%ebp
  402a91:	31 e9                	xor    %ebp,%ecx
  402a93:	29 ca                	sub    %ecx,%edx
  402a95:	c1 e9 0f             	shr    $0xf,%ecx
  402a98:	31 d1                	xor    %edx,%ecx
  402a9a:	89 8b 3c 01 00 00    	mov    %ecx,0x13c(%rbx)
  402aa0:	89 cd                	mov    %ecx,%ebp
  402aa2:	48 85 c0             	test   %rax,%rax
  402aa5:	0f 84 a7 01 00 00    	je     402c52 <gib_add_symbol+0x2c2>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 29)
  402aab:	48 8b 90 08 01 00 00 	mov    0x108(%rax),%rdx
  402ab2:	4c 8d ab 08 01 00 00 	lea    0x108(%rbx),%r13
  402ab9:	48 c7 83 18 01 00 00 	movq   $0x0,0x118(%rbx)
  402ac0:	00 00 00 00 
  402ac4:	48 89 93 08 01 00 00 	mov    %rdx,0x108(%rbx)
  402acb:	48 8b 90 08 01 00 00 	mov    0x108(%rax),%rdx
  402ad2:	48 8b 4a 18          	mov    0x18(%rdx),%rcx
  402ad6:	48 89 ce             	mov    %rcx,%rsi
  402ad9:	48 2b 72 20          	sub    0x20(%rdx),%rsi
  402add:	48 89 b3 10 01 00 00 	mov    %rsi,0x110(%rbx)
  402ae4:	48 89 59 10          	mov    %rbx,0x10(%rcx)
  402ae8:	4c 89 6a 18          	mov    %r13,0x18(%rdx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 37)
  402aec:	48 8b 90 08 01 00 00 	mov    0x108(%rax),%rdx
  402af3:	8b 42 08             	mov    0x8(%rdx),%eax
  402af6:	83 42 10 01          	addl   $0x1,0x10(%rdx)
  402afa:	83 e8 01             	sub    $0x1,%eax
  402afd:	21 e8                	and    %ebp,%eax
  402aff:	48 c1 e0 04          	shl    $0x4,%rax
  402b03:	48 03 02             	add    (%rdx),%rax
  402b06:	8b 78 08             	mov    0x8(%rax),%edi
  402b09:	48 8b 08             	mov    (%rax),%rcx
  402b0c:	8d 57 01             	lea    0x1(%rdi),%edx
  402b0f:	89 50 08             	mov    %edx,0x8(%rax)
  402b12:	48 89 8b 28 01 00 00 	mov    %rcx,0x128(%rbx)
  402b19:	48 c7 83 20 01 00 00 	movq   $0x0,0x120(%rbx)
  402b20:	00 00 00 00 
  402b24:	48 85 c9             	test   %rcx,%rcx
  402b27:	74 04                	je     402b2d <gib_add_symbol+0x19d>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 38)
  402b29:	4c 89 69 18          	mov    %r13,0x18(%rcx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 40)
  402b2d:	4c 89 28             	mov    %r13,(%rax)
  402b30:	8b 40 0c             	mov    0xc(%rax),%eax
  402b33:	8d 44 80 05          	lea    0x5(%rax,%rax,4),%eax
  402b37:	01 c0                	add    %eax,%eax
  402b39:	39 c2                	cmp    %eax,%edx
  402b3b:	72 0d                	jb     402b4a <gib_add_symbol+0x1ba>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 41)
  402b3d:	48 8b ab 08 01 00 00 	mov    0x108(%rbx),%rbp
  402b44:	83 7d 34 00          	cmpl   $0x0,0x34(%rbp)
  402b48:	74 19                	je     402b63 <gib_add_symbol+0x1d3>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:417
    if (idx > gib_global_gensym_counter) {
  402b4a:	4c 39 25 ff 25 00 00 	cmp    %r12,0x25ff(%rip)        # 405150 <gib_global_gensym_counter>
  402b51:	73 07                	jae    402b5a <gib_add_symbol+0x1ca>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:418
        gib_global_gensym_counter = idx;
  402b53:	4c 89 25 f6 25 00 00 	mov    %r12,0x25f6(%rip)        # 405150 <gib_global_gensym_counter>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:421
}
  402b5a:	5b                   	pop    %rbx
  402b5b:	5d                   	pop    %rbp
  402b5c:	41 5c                	pop    %r12
  402b5e:	41 5d                	pop    %r13
  402b60:	41 5e                	pop    %r14
  402b62:	c3                   	ret
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 42)
    HASH_ADD(hh, global_sym_table, idx, sizeof(GibSym), s);
  402b63:	8b 7d 08             	mov    0x8(%rbp),%edi
  402b66:	be 01 00 00 00       	mov    $0x1,%esi
  402b6b:	48 c1 e7 05          	shl    $0x5,%rdi
  402b6f:	e8 8c e5 ff ff       	call   401100 <calloc@plt>
  402b74:	49 89 c5             	mov    %rax,%r13
  402b77:	48 85 c0             	test   %rax,%rax
  402b7a:	0f 84 a4 01 00 00    	je     402d24 <gib_add_symbol+0x394>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 44)
  402b80:	8b 55 08             	mov    0x8(%rbp),%edx
  402b83:	8b 45 10             	mov    0x10(%rbp),%eax
  402b86:	c7 45 2c 00 00 00 00 	movl   $0x0,0x2c(%rbp)
  402b8d:	8b 7d 0c             	mov    0xc(%rbp),%edi
  402b90:	44 8d 44 12 ff       	lea    -0x1(%rdx,%rdx,1),%r8d
  402b95:	8d 4f 01             	lea    0x1(%rdi),%ecx
  402b98:	89 c7                	mov    %eax,%edi
  402b9a:	44 21 c0             	and    %r8d,%eax
  402b9d:	d3 ef                	shr    %cl,%edi
  402b9f:	83 f8 01             	cmp    $0x1,%eax
  402ba2:	83 df ff             	sbb    $0xffffffff,%edi
  402ba5:	89 7d 28             	mov    %edi,0x28(%rbp)
  402ba8:	85 d2                	test   %edx,%edx
  402baa:	0f 84 26 01 00 00    	je     402cd6 <gib_add_symbol+0x346>
  402bb0:	48 8b 45 00          	mov    0x0(%rbp),%rax
  402bb4:	44 8d 52 ff          	lea    -0x1(%rdx),%r10d
  402bb8:	49 c1 e2 04          	shl    $0x4,%r10
  402bbc:	4c 8d 48 10          	lea    0x10(%rax),%r9
  402bc0:	4d 01 ca             	add    %r9,%r10
  402bc3:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 63)
  402bc8:	48 8b 08             	mov    (%rax),%rcx
  402bcb:	48 85 c9             	test   %rcx,%rcx
  402bce:	75 17                	jne    402be7 <gib_add_symbol+0x257>
  402bd0:	eb 6b                	jmp    402c3d <gib_add_symbol+0x2ad>
  402bd2:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 56)
  402bd8:	48 89 4a 18          	mov    %rcx,0x18(%rdx)
  402bdc:	48 89 08             	mov    %rcx,(%rax)
  402bdf:	48 85 f6             	test   %rsi,%rsi
  402be2:	74 59                	je     402c3d <gib_add_symbol+0x2ad>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416
  402be4:	48 89 f1             	mov    %rsi,%rcx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 60)
  402be7:	44 89 c0             	mov    %r8d,%eax
  402bea:	23 41 34             	and    0x34(%rcx),%eax
  402bed:	48 8b 71 20          	mov    0x20(%rcx),%rsi
  402bf1:	48 c1 e0 04          	shl    $0x4,%rax
  402bf5:	4c 01 e8             	add    %r13,%rax
  402bf8:	8b 50 08             	mov    0x8(%rax),%edx
  402bfb:	83 c2 01             	add    $0x1,%edx
  402bfe:	89 50 08             	mov    %edx,0x8(%rax)
  402c01:	39 d7                	cmp    %edx,%edi
  402c03:	73 1c                	jae    402c21 <gib_add_symbol+0x291>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 51)
  402c05:	44 8b 58 0c          	mov    0xc(%rax),%r11d
  402c09:	41 89 fe             	mov    %edi,%r14d
  402c0c:	83 45 2c 01          	addl   $0x1,0x2c(%rbp)
  402c10:	45 0f af f3          	imul   %r11d,%r14d
  402c14:	44 39 f2             	cmp    %r14d,%edx
  402c17:	76 08                	jbe    402c21 <gib_add_symbol+0x291>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 53)
  402c19:	41 83 c3 01          	add    $0x1,%r11d
  402c1d:	44 89 58 0c          	mov    %r11d,0xc(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 55)
  402c21:	48 8b 10             	mov    (%rax),%rdx
  402c24:	48 c7 41 18 00 00 00 	movq   $0x0,0x18(%rcx)
  402c2b:	00 
  402c2c:	48 89 51 20          	mov    %rdx,0x20(%rcx)
  402c30:	48 85 d2             	test   %rdx,%rdx
  402c33:	75 a3                	jne    402bd8 <gib_add_symbol+0x248>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 58)
  402c35:	48 89 08             	mov    %rcx,(%rax)
  402c38:	48 85 f6             	test   %rsi,%rsi
  402c3b:	75 a7                	jne    402be4 <gib_add_symbol+0x254>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416
  402c3d:	4c 89 c8             	mov    %r9,%rax
  402c40:	4d 39 d1             	cmp    %r10,%r9
  402c43:	0f 84 8d 00 00 00    	je     402cd6 <gib_add_symbol+0x346>
  402c49:	49 83 c1 10          	add    $0x10,%r9
  402c4d:	e9 76 ff ff ff       	jmp    402bc8 <gib_add_symbol+0x238>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 28)
  402c52:	66 0f ef c0          	pxor   %xmm0,%xmm0
  402c56:	bf 40 00 00 00       	mov    $0x40,%edi
  402c5b:	0f 11 83 10 01 00 00 	movups %xmm0,0x110(%rbx)
  402c62:	e8 f9 e4 ff ff       	call   401160 <malloc@plt>
  402c67:	48 89 83 08 01 00 00 	mov    %rax,0x108(%rbx)
  402c6e:	49 89 c6             	mov    %rax,%r14
  402c71:	48 85 c0             	test   %rax,%rax
  402c74:	0f 84 aa 00 00 00    	je     402d24 <gib_add_symbol+0x394>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 31)
  402c7a:	48 8d 50 10          	lea    0x10(%rax),%rdx
  402c7e:	b9 0c 00 00 00       	mov    $0xc,%ecx
  402c83:	31 c0                	xor    %eax,%eax
  402c85:	be 01 00 00 00       	mov    $0x1,%esi
  402c8a:	48 89 d7             	mov    %rdx,%rdi
  402c8d:	4c 8d ab 08 01 00 00 	lea    0x108(%rbx),%r13
  402c94:	f3 ab                	rep stos %eax,%es:(%rdi)
  402c96:	48 8b 05 53 0c 00 00 	mov    0xc53(%rip),%rax        # 4038f0 <__PRETTY_FUNCTION__.3+0x20>
  402c9d:	4d 89 6e 18          	mov    %r13,0x18(%r14)
  402ca1:	bf 00 02 00 00       	mov    $0x200,%edi
  402ca6:	49 c7 46 20 08 01 00 	movq   $0x108,0x20(%r14)
  402cad:	00 
  402cae:	49 89 46 08          	mov    %rax,0x8(%r14)
  402cb2:	e8 49 e4 ff ff       	call   401100 <calloc@plt>
  402cb7:	41 c7 46 38 e1 1f 11 	movl   $0xa0111fe1,0x38(%r14)
  402cbe:	a0 
  402cbf:	49 89 06             	mov    %rax,(%r14)
  402cc2:	48 85 c0             	test   %rax,%rax
  402cc5:	74 5d                	je     402d24 <gib_add_symbol+0x394>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 33)
  402cc7:	48 89 1d 8a 24 00 00 	mov    %rbx,0x248a(%rip)        # 405158 <global_sym_table>
  402cce:	48 89 d8             	mov    %rbx,%rax
  402cd1:	e9 16 fe ff ff       	jmp    402aec <gib_add_symbol+0x15c>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416
  402cd6:	48 8b 7d 00          	mov    0x0(%rbp),%rdi
  402cda:	e8 51 e3 ff ff       	call   401030 <free@plt>
  402cdf:	48 8b 83 08 01 00 00 	mov    0x108(%rbx),%rax
  402ce6:	8b 50 10             	mov    0x10(%rax),%edx
  402ce9:	83 40 0c 01          	addl   $0x1,0xc(%rax)
  402ced:	4c 89 28             	mov    %r13,(%rax)
  402cf0:	d1 60 08             	shll   $1,0x8(%rax)
  402cf3:	d1 ea                	shr    $1,%edx
  402cf5:	39 50 2c             	cmp    %edx,0x2c(%rax)
  402cf8:	76 1e                	jbe    402d18 <gib_add_symbol+0x388>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 65)
  402cfa:	8b 58 30             	mov    0x30(%rax),%ebx
  402cfd:	8d 53 01             	lea    0x1(%rbx),%edx
  402d00:	89 50 30             	mov    %edx,0x30(%rax)
  402d03:	83 fa 01             	cmp    $0x1,%edx
  402d06:	0f 86 3e fe ff ff    	jbe    402b4a <gib_add_symbol+0x1ba>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 69)
  402d0c:	c7 40 34 01 00 00 00 	movl   $0x1,0x34(%rax)
  402d13:	e9 32 fe ff ff       	jmp    402b4a <gib_add_symbol+0x1ba>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416
  402d18:	c7 40 30 00 00 00 00 	movl   $0x0,0x30(%rax)
  402d1f:	e9 26 fe ff ff       	jmp    402b4a <gib_add_symbol+0x1ba>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 30)
  402d24:	83 cf ff             	or     $0xffffffff,%edi
  402d27:	e8 84 e4 ff ff       	call   4011b0 <exit@plt>
  402d2c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402d30 <gib_check_rust_struct_sizes>:
gib_check_rust_struct_sizes():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:994
{
  402d30:	55                   	push   %rbp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  402d31:	bf 38 00 00 00       	mov    $0x38,%edi
  402d36:	e8 25 e4 ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1004
    gib_get_rust_struct_sizes(stack, frame, nursery, generation, reg_info, footer, gc_stats);
  402d3b:	48 83 ec 08          	sub    $0x8,%rsp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  402d3f:	48 89 c5             	mov    %rax,%rbp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:999
    nursery = (size_t *) ((char *) frame + sizeof(size_t));
  402d42:	48 8d 50 10          	lea    0x10(%rax),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1000
    generation = (size_t *) ((char *) nursery + sizeof(size_t));
  402d46:	48 8d 48 18          	lea    0x18(%rax),%rcx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:998
    frame = (size_t *) ((char *) stack + sizeof(size_t));
  402d4a:	48 8d 70 08          	lea    0x8(%rax),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1003
    gc_stats = (size_t *) ((char *) footer + sizeof(size_t));
  402d4e:	48 83 c0 30          	add    $0x30,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1004
    gib_get_rust_struct_sizes(stack, frame, nursery, generation, reg_info, footer, gc_stats);
  402d52:	4c 8d 4d 28          	lea    0x28(%rbp),%r9
  402d56:	48 89 ef             	mov    %rbp,%rdi
  402d59:	50                   	push   %rax
  402d5a:	4c 8d 45 20          	lea    0x20(%rbp),%r8
  402d5e:	e8 2d e4 ff ff       	call   401190 <gib_get_rust_struct_sizes@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1007
    assert(*stack == sizeof(GibShadowstack));
  402d63:	48 83 7d 00 18       	cmpq   $0x18,0x0(%rbp)
  402d68:	58                   	pop    %rax
  402d69:	5a                   	pop    %rdx
  402d6a:	75 42                	jne    402dae <gib_check_rust_struct_sizes+0x7e>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1008
    assert(*frame == sizeof(GibShadowstackFrame));
  402d6c:	48 83 7d 08 18       	cmpq   $0x18,0x8(%rbp)
  402d71:	0f 85 cd 00 00 00    	jne    402e44 <gib_check_rust_struct_sizes+0x114>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1009
    assert(*nursery == sizeof(GibNursery));
  402d77:	48 83 7d 10 20       	cmpq   $0x20,0x10(%rbp)
  402d7c:	0f 85 a9 00 00 00    	jne    402e2b <gib_check_rust_struct_sizes+0xfb>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1010
    assert(*generation == sizeof(GibOldgen));
  402d82:	48 83 7d 18 18       	cmpq   $0x18,0x18(%rbp)
  402d87:	0f 85 85 00 00 00    	jne    402e12 <gib_check_rust_struct_sizes+0xe2>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1011
    assert(*reg_info == sizeof(GibRegionInfo));
  402d8d:	48 83 7d 20 20       	cmpq   $0x20,0x20(%rbp)
  402d92:	75 65                	jne    402df9 <gib_check_rust_struct_sizes+0xc9>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1012
    assert(*footer == sizeof(GibOldgenChunkFooter));
  402d94:	48 83 7d 28 18       	cmpq   $0x18,0x28(%rbp)
  402d99:	75 45                	jne    402de0 <gib_check_rust_struct_sizes+0xb0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1013
    assert(*gc_stats == sizeof(GibGcStats));
  402d9b:	48 81 7d 30 f0 00 00 	cmpq   $0xf0,0x30(%rbp)
  402da2:	00 
  402da3:	75 22                	jne    402dc7 <gib_check_rust_struct_sizes+0x97>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:242
void gib_free(void *ptr) { free(ptr); }
  402da5:	48 89 ef             	mov    %rbp,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1019
}
  402da8:	5d                   	pop    %rbp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:242
void gib_free(void *ptr) { free(ptr); }
  402da9:	e9 82 e2 ff ff       	jmp    401030 <free@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1007 (discriminator 1)
    assert(*stack == sizeof(GibShadowstack));
  402dae:	b9 d0 38 40 00       	mov    $0x4038d0,%ecx
  402db3:	ba ef 03 00 00       	mov    $0x3ef,%edx
  402db8:	be a8 33 40 00       	mov    $0x4033a8,%esi
  402dbd:	bf e8 33 40 00       	mov    $0x4033e8,%edi
  402dc2:	e8 19 e3 ff ff       	call   4010e0 <__assert_fail@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1013 (discriminator 1)
    assert(*gc_stats == sizeof(GibGcStats));
  402dc7:	b9 d0 38 40 00       	mov    $0x4038d0,%ecx
  402dcc:	ba f5 03 00 00       	mov    $0x3f5,%edx
  402dd1:	be a8 33 40 00       	mov    $0x4033a8,%esi
  402dd6:	bf d0 34 40 00       	mov    $0x4034d0,%edi
  402ddb:	e8 00 e3 ff ff       	call   4010e0 <__assert_fail@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1012 (discriminator 1)
    assert(*footer == sizeof(GibOldgenChunkFooter));
  402de0:	b9 d0 38 40 00       	mov    $0x4038d0,%ecx
  402de5:	ba f4 03 00 00       	mov    $0x3f4,%edx
  402dea:	be a8 33 40 00       	mov    $0x4033a8,%esi
  402def:	bf a8 34 40 00       	mov    $0x4034a8,%edi
  402df4:	e8 e7 e2 ff ff       	call   4010e0 <__assert_fail@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1011 (discriminator 1)
    assert(*reg_info == sizeof(GibRegionInfo));
  402df9:	b9 d0 38 40 00       	mov    $0x4038d0,%ecx
  402dfe:	ba f3 03 00 00       	mov    $0x3f3,%edx
  402e03:	be a8 33 40 00       	mov    $0x4033a8,%esi
  402e08:	bf 80 34 40 00       	mov    $0x403480,%edi
  402e0d:	e8 ce e2 ff ff       	call   4010e0 <__assert_fail@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1010 (discriminator 1)
    assert(*generation == sizeof(GibOldgen));
  402e12:	b9 d0 38 40 00       	mov    $0x4038d0,%ecx
  402e17:	ba f2 03 00 00       	mov    $0x3f2,%edx
  402e1c:	be a8 33 40 00       	mov    $0x4033a8,%esi
  402e21:	bf 58 34 40 00       	mov    $0x403458,%edi
  402e26:	e8 b5 e2 ff ff       	call   4010e0 <__assert_fail@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1009 (discriminator 1)
    assert(*nursery == sizeof(GibNursery));
  402e2b:	b9 d0 38 40 00       	mov    $0x4038d0,%ecx
  402e30:	ba f1 03 00 00       	mov    $0x3f1,%edx
  402e35:	be a8 33 40 00       	mov    $0x4033a8,%esi
  402e3a:	bf 38 34 40 00       	mov    $0x403438,%edi
  402e3f:	e8 9c e2 ff ff       	call   4010e0 <__assert_fail@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1008 (discriminator 1)
    assert(*frame == sizeof(GibShadowstackFrame));
  402e44:	b9 d0 38 40 00       	mov    $0x4038d0,%ecx
  402e49:	ba f0 03 00 00       	mov    $0x3f0,%edx
  402e4e:	be a8 33 40 00       	mov    $0x4033a8,%esi
  402e53:	bf 10 34 40 00       	mov    $0x403410,%edi
  402e58:	e8 83 e2 ff ff       	call   4010e0 <__assert_fail@plt>
  402e5d:	0f 1f 00             	nopl   (%rax)

0000000000402e60 <gib_print_symbol.isra.0>:
gib_print_symbol.isra.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:458
int gib_print_symbol(GibSym idx)
  402e60:	48 83 ec 18          	sub    $0x18,%rsp
  402e64:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:460
    if (idx == comma_symbol) {
  402e69:	48 83 ff ff          	cmp    $0xffffffffffffffff,%rdi
  402e6d:	0f 84 65 01 00 00    	je     402fd8 <gib_print_symbol.isra.0+0x178>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:472
        HASH_FIND(hh, global_sym_table, &idx, sizeof(GibSym), s);
  402e73:	48 8b 35 de 22 00 00 	mov    0x22de(%rip),%rsi        # 405158 <global_sym_table>
  402e7a:	49 89 f8             	mov    %rdi,%r8
  402e7d:	48 85 f6             	test   %rsi,%rsi
  402e80:	0f 84 3a 01 00 00    	je     402fc0 <gib_print_symbol.isra.0+0x160>
  402e86:	0f b6 44 24 0f       	movzbl 0xf(%rsp),%eax
  402e8b:	0f b6 54 24 0c       	movzbl 0xc(%rsp),%edx
  402e90:	0f b6 7c 24 0e       	movzbl 0xe(%rsp),%edi
  402e95:	c1 e0 18             	shl    $0x18,%eax
  402e98:	c1 e7 10             	shl    $0x10,%edi
  402e9b:	8d 94 10 b9 79 37 9e 	lea    -0x61c88647(%rax,%rdx,1),%edx
  402ea2:	8d 04 3a             	lea    (%rdx,%rdi,1),%eax
  402ea5:	0f b6 7c 24 0d       	movzbl 0xd(%rsp),%edi
  402eaa:	0f b6 54 24 08       	movzbl 0x8(%rsp),%edx
  402eaf:	c1 e7 08             	shl    $0x8,%edi
  402eb2:	01 c7                	add    %eax,%edi
  402eb4:	0f b6 44 24 0b       	movzbl 0xb(%rsp),%eax
  402eb9:	c1 e0 18             	shl    $0x18,%eax
  402ebc:	8d 8c 02 c2 ba 49 9f 	lea    -0x60b6453e(%rdx,%rax,1),%ecx
  402ec3:	0f b6 44 24 0a       	movzbl 0xa(%rsp),%eax
  402ec8:	c1 e0 10             	shl    $0x10,%eax
  402ecb:	8d 14 01             	lea    (%rcx,%rax,1),%edx
  402ece:	0f b6 44 24 09       	movzbl 0x9(%rsp),%eax
  402ed3:	b9 f7 be ed fe       	mov    $0xfeedbef7,%ecx
  402ed8:	c1 e0 08             	shl    $0x8,%eax
  402edb:	01 d0                	add    %edx,%eax
  402edd:	29 f8                	sub    %edi,%eax
  402edf:	81 c7 09 41 12 01    	add    $0x1124109,%edi
  402ee5:	35 6d f7 07 00       	xor    $0x7f76d,%eax
  402eea:	89 c2                	mov    %eax,%edx
  402eec:	29 c7                	sub    %eax,%edi
  402eee:	29 c1                	sub    %eax,%ecx
  402ef0:	c1 e2 08             	shl    $0x8,%edx
  402ef3:	31 fa                	xor    %edi,%edx
  402ef5:	29 d1                	sub    %edx,%ecx
  402ef7:	29 d0                	sub    %edx,%eax
  402ef9:	89 cf                	mov    %ecx,%edi
  402efb:	89 d1                	mov    %edx,%ecx
  402efd:	c1 e9 0d             	shr    $0xd,%ecx
  402f00:	31 f9                	xor    %edi,%ecx
  402f02:	89 cf                	mov    %ecx,%edi
  402f04:	29 c8                	sub    %ecx,%eax
  402f06:	29 ca                	sub    %ecx,%edx
  402f08:	c1 ef 0c             	shr    $0xc,%edi
  402f0b:	31 f8                	xor    %edi,%eax
  402f0d:	89 d7                	mov    %edx,%edi
  402f0f:	89 c2                	mov    %eax,%edx
  402f11:	29 c7                	sub    %eax,%edi
  402f13:	29 c1                	sub    %eax,%ecx
  402f15:	c1 e2 10             	shl    $0x10,%edx
  402f18:	31 fa                	xor    %edi,%edx
  402f1a:	29 d1                	sub    %edx,%ecx
  402f1c:	29 d0                	sub    %edx,%eax
  402f1e:	89 cf                	mov    %ecx,%edi
  402f20:	89 d1                	mov    %edx,%ecx
  402f22:	c1 e9 05             	shr    $0x5,%ecx
  402f25:	31 f9                	xor    %edi,%ecx
  402f27:	89 cf                	mov    %ecx,%edi
  402f29:	29 c8                	sub    %ecx,%eax
  402f2b:	c1 ef 03             	shr    $0x3,%edi
  402f2e:	29 ca                	sub    %ecx,%edx
  402f30:	31 c7                	xor    %eax,%edi
  402f32:	89 d0                	mov    %edx,%eax
  402f34:	89 fa                	mov    %edi,%edx
  402f36:	29 f8                	sub    %edi,%eax
  402f38:	c1 e2 0a             	shl    $0xa,%edx
  402f3b:	31 c2                	xor    %eax,%edx
  402f3d:	89 c8                	mov    %ecx,%eax
  402f3f:	48 8b 8e 08 01 00 00 	mov    0x108(%rsi),%rcx
  402f46:	29 f8                	sub    %edi,%eax
  402f48:	8b 79 08             	mov    0x8(%rcx),%edi
  402f4b:	29 d0                	sub    %edx,%eax
  402f4d:	c1 ea 0f             	shr    $0xf,%edx
  402f50:	31 d0                	xor    %edx,%eax
  402f52:	8d 57 ff             	lea    -0x1(%rdi),%edx
  402f55:	21 c2                	and    %eax,%edx
  402f57:	48 c1 e2 04          	shl    $0x4,%rdx
  402f5b:	48 03 11             	add    (%rcx),%rdx
  402f5e:	48 8b 32             	mov    (%rdx),%rsi
  402f61:	48 85 f6             	test   %rsi,%rsi
  402f64:	74 5a                	je     402fc0 <gib_print_symbol.isra.0+0x160>
  402f66:	48 8b 49 20          	mov    0x20(%rcx),%rcx
  402f6a:	48 89 ca             	mov    %rcx,%rdx
  402f6d:	48 29 ce             	sub    %rcx,%rsi
  402f70:	48 f7 da             	neg    %rdx
  402f73:	eb 12                	jmp    402f87 <gib_print_symbol.isra.0+0x127>
  402f75:	0f 1f 00             	nopl   (%rax)
  402f78:	48 8b b6 28 01 00 00 	mov    0x128(%rsi),%rsi
  402f7f:	48 85 f6             	test   %rsi,%rsi
  402f82:	74 3c                	je     402fc0 <gib_print_symbol.isra.0+0x160>
  402f84:	48 01 d6             	add    %rdx,%rsi
  402f87:	3b 86 3c 01 00 00    	cmp    0x13c(%rsi),%eax
  402f8d:	75 e9                	jne    402f78 <gib_print_symbol.isra.0+0x118>
  402f8f:	83 be 38 01 00 00 08 	cmpl   $0x8,0x138(%rsi)
  402f96:	75 e0                	jne    402f78 <gib_print_symbol.isra.0+0x118>
  402f98:	48 8b 8e 30 01 00 00 	mov    0x130(%rsi),%rcx
  402f9f:	48 8b 09             	mov    (%rcx),%rcx
  402fa2:	48 39 4c 24 08       	cmp    %rcx,0x8(%rsp)
  402fa7:	75 cf                	jne    402f78 <gib_print_symbol.isra.0+0x118>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:476
            return printf("%s", s->value);
  402fa9:	48 83 c6 08          	add    $0x8,%rsi
  402fad:	bf 40 37 40 00       	mov    $0x403740,%edi
  402fb2:	31 c0                	xor    %eax,%eax
  402fb4:	e8 07 e1 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:480
}
  402fb9:	48 83 c4 18          	add    $0x18,%rsp
  402fbd:	c3                   	ret
  402fbe:	66 90                	xchg   %ax,%ax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:474
            return printf("%" PRId64, idx);
  402fc0:	4c 89 c6             	mov    %r8,%rsi
  402fc3:	bf 3c 37 40 00       	mov    $0x40373c,%edi
  402fc8:	31 c0                	xor    %eax,%eax
  402fca:	e8 f1 e0 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:480
}
  402fcf:	48 83 c4 18          	add    $0x18,%rsp
  402fd3:	c3                   	ret
  402fd4:	0f 1f 40 00          	nopl   0x0(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:461
        return printf(",");
  402fd8:	bf 2c 00 00 00       	mov    $0x2c,%edi
  402fdd:	e8 5e e0 ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:480
}
  402fe2:	48 83 c4 18          	add    $0x18,%rsp
  402fe6:	c3                   	ret

Disassembly of section .fini:

0000000000402fe8 <_fini>:
_fini():
  402fe8:	f3 0f 1e fa          	endbr64
  402fec:	48 83 ec 08          	sub    $0x8,%rsp
  402ff0:	48 83 c4 08          	add    $0x8,%rsp
  402ff4:	c3                   	ret
