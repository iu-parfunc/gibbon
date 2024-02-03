
InsertionNoTailCallOpt.exe:     file format elf64-x86-64


Disassembly of section .init:

0000000000401000 <_init>:
_init():
  401000:	f3 0f 1e fa          	endbr64
  401004:	48 83 ec 08          	sub    $0x8,%rsp
  401008:	48 8b 05 d1 4f 00 00 	mov    0x4fd1(%rip),%rax        # 405fe0 <__gmon_start__@Base>
  40100f:	48 85 c0             	test   %rax,%rax
  401012:	74 02                	je     401016 <_init+0x16>
  401014:	ff d0                	call   *%rax
  401016:	48 83 c4 08          	add    $0x8,%rsp
  40101a:	c3                   	ret

Disassembly of section .plt:

0000000000401020 <free@plt-0x10>:
  401020:	ff 35 ca 4f 00 00    	push   0x4fca(%rip)        # 405ff0 <_GLOBAL_OFFSET_TABLE_+0x8>
  401026:	ff 25 cc 4f 00 00    	jmp    *0x4fcc(%rip)        # 405ff8 <_GLOBAL_OFFSET_TABLE_+0x10>
  40102c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401030 <free@plt>:
  401030:	ff 25 ca 4f 00 00    	jmp    *0x4fca(%rip)        # 406000 <free@GLIBC_2.2.5>
  401036:	68 00 00 00 00       	push   $0x0
  40103b:	e9 e0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401040 <putchar@plt>:
  401040:	ff 25 c2 4f 00 00    	jmp    *0x4fc2(%rip)        # 406008 <putchar@GLIBC_2.2.5>
  401046:	68 01 00 00 00       	push   $0x1
  40104b:	e9 d0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401050 <strncpy@plt>:
  401050:	ff 25 ba 4f 00 00    	jmp    *0x4fba(%rip)        # 406010 <strncpy@GLIBC_2.2.5>
  401056:	68 02 00 00 00       	push   $0x2
  40105b:	e9 c0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401060 <strcpy@plt>:
  401060:	ff 25 b2 4f 00 00    	jmp    *0x4fb2(%rip)        # 406018 <strcpy@GLIBC_2.2.5>
  401066:	68 03 00 00 00       	push   $0x3
  40106b:	e9 b0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401070 <gib_init_zcts@plt>:
  401070:	ff 25 aa 4f 00 00    	jmp    *0x4faa(%rip)        # 406020 <gib_init_zcts@Base>
  401076:	68 04 00 00 00       	push   $0x4
  40107b:	e9 a0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401080 <puts@plt>:
  401080:	ff 25 a2 4f 00 00    	jmp    *0x4fa2(%rip)        # 406028 <puts@GLIBC_2.2.5>
  401086:	68 05 00 00 00       	push   $0x5
  40108b:	e9 90 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401090 <qsort@plt>:
  401090:	ff 25 9a 4f 00 00    	jmp    *0x4f9a(%rip)        # 406030 <qsort@GLIBC_2.2.5>
  401096:	68 06 00 00 00       	push   $0x6
  40109b:	e9 80 ff ff ff       	jmp    401020 <_init+0x20>

00000000004010a0 <clock_gettime@plt>:
  4010a0:	ff 25 92 4f 00 00    	jmp    *0x4f92(%rip)        # 406038 <clock_gettime@GLIBC_2.17>
  4010a6:	68 07 00 00 00       	push   $0x7
  4010ab:	e9 70 ff ff ff       	jmp    401020 <_init+0x20>

00000000004010b0 <strlen@plt>:
  4010b0:	ff 25 8a 4f 00 00    	jmp    *0x4f8a(%rip)        # 406040 <strlen@GLIBC_2.2.5>
  4010b6:	68 08 00 00 00       	push   $0x8
  4010bb:	e9 60 ff ff ff       	jmp    401020 <_init+0x20>

00000000004010c0 <printf@plt>:
  4010c0:	ff 25 82 4f 00 00    	jmp    *0x4f82(%rip)        # 406048 <printf@GLIBC_2.2.5>
  4010c6:	68 09 00 00 00       	push   $0x9
  4010cb:	e9 50 ff ff ff       	jmp    401020 <_init+0x20>

00000000004010d0 <gib_gc_cleanup@plt>:
  4010d0:	ff 25 7a 4f 00 00    	jmp    *0x4f7a(%rip)        # 406050 <gib_gc_cleanup@Base>
  4010d6:	68 0a 00 00 00       	push   $0xa
  4010db:	e9 40 ff ff ff       	jmp    401020 <_init+0x20>

00000000004010e0 <__assert_fail@plt>:
  4010e0:	ff 25 72 4f 00 00    	jmp    *0x4f72(%rip)        # 406058 <__assert_fail@GLIBC_2.2.5>
  4010e6:	68 0b 00 00 00       	push   $0xb
  4010eb:	e9 30 ff ff ff       	jmp    401020 <_init+0x20>

00000000004010f0 <gib_info_table_initialize@plt>:
  4010f0:	ff 25 6a 4f 00 00    	jmp    *0x4f6a(%rip)        # 406060 <gib_info_table_initialize@Base>
  4010f6:	68 0c 00 00 00       	push   $0xc
  4010fb:	e9 20 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401100 <calloc@plt>:
  401100:	ff 25 62 4f 00 00    	jmp    *0x4f62(%rip)        # 406068 <calloc@GLIBC_2.2.5>
  401106:	68 0d 00 00 00       	push   $0xd
  40110b:	e9 10 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401110 <strcmp@plt>:
  401110:	ff 25 5a 4f 00 00    	jmp    *0x4f5a(%rip)        # 406070 <strcmp@GLIBC_2.2.5>
  401116:	68 0e 00 00 00       	push   $0xe
  40111b:	e9 00 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401120 <strtoll@plt>:
  401120:	ff 25 52 4f 00 00    	jmp    *0x4f52(%rip)        # 406078 <strtoll@GLIBC_2.2.5>
  401126:	68 0f 00 00 00       	push   $0xf
  40112b:	e9 f0 fe ff ff       	jmp    401020 <_init+0x20>

0000000000401130 <fprintf@plt>:
  401130:	ff 25 4a 4f 00 00    	jmp    *0x4f4a(%rip)        # 406080 <fprintf@GLIBC_2.2.5>
  401136:	68 10 00 00 00       	push   $0x10
  40113b:	e9 e0 fe ff ff       	jmp    401020 <_init+0x20>

0000000000401140 <memcpy@plt>:
  401140:	ff 25 42 4f 00 00    	jmp    *0x4f42(%rip)        # 406088 <memcpy@GLIBC_2.14>
  401146:	68 11 00 00 00       	push   $0x11
  40114b:	e9 d0 fe ff ff       	jmp    401020 <_init+0x20>

0000000000401150 <gib_print_rust_gc_config@plt>:
  401150:	ff 25 3a 4f 00 00    	jmp    *0x4f3a(%rip)        # 406090 <gib_print_rust_gc_config@Base>
  401156:	68 12 00 00 00       	push   $0x12
  40115b:	e9 c0 fe ff ff       	jmp    401020 <_init+0x20>

0000000000401160 <malloc@plt>:
  401160:	ff 25 32 4f 00 00    	jmp    *0x4f32(%rip)        # 406098 <malloc@GLIBC_2.2.5>
  401166:	68 13 00 00 00       	push   $0x13
  40116b:	e9 b0 fe ff ff       	jmp    401020 <_init+0x20>

0000000000401170 <fflush@plt>:
  401170:	ff 25 2a 4f 00 00    	jmp    *0x4f2a(%rip)        # 4060a0 <fflush@GLIBC_2.2.5>
  401176:	68 14 00 00 00       	push   $0x14
  40117b:	e9 a0 fe ff ff       	jmp    401020 <_init+0x20>

0000000000401180 <setrlimit@plt>:
  401180:	ff 25 22 4f 00 00    	jmp    *0x4f22(%rip)        # 4060a8 <setrlimit@GLIBC_2.2.5>
  401186:	68 15 00 00 00       	push   $0x15
  40118b:	e9 90 fe ff ff       	jmp    401020 <_init+0x20>

0000000000401190 <gib_get_rust_struct_sizes@plt>:
  401190:	ff 25 1a 4f 00 00    	jmp    *0x4f1a(%rip)        # 4060b0 <gib_get_rust_struct_sizes@Base>
  401196:	68 16 00 00 00       	push   $0x16
  40119b:	e9 80 fe ff ff       	jmp    401020 <_init+0x20>

00000000004011a0 <gib_info_table_finalize@plt>:
  4011a0:	ff 25 12 4f 00 00    	jmp    *0x4f12(%rip)        # 4060b8 <gib_info_table_finalize@Base>
  4011a6:	68 17 00 00 00       	push   $0x17
  4011ab:	e9 70 fe ff ff       	jmp    401020 <_init+0x20>

00000000004011b0 <exit@plt>:
  4011b0:	ff 25 0a 4f 00 00    	jmp    *0x4f0a(%rip)        # 4060c0 <exit@GLIBC_2.2.5>
  4011b6:	68 18 00 00 00       	push   $0x18
  4011bb:	e9 60 fe ff ff       	jmp    401020 <_init+0x20>

00000000004011c0 <fwrite@plt>:
  4011c0:	ff 25 02 4f 00 00    	jmp    *0x4f02(%rip)        # 4060c8 <fwrite@GLIBC_2.2.5>
  4011c6:	68 19 00 00 00       	push   $0x19
  4011cb:	e9 50 fe ff ff       	jmp    401020 <_init+0x20>

00000000004011d0 <getrlimit@plt>:
  4011d0:	ff 25 fa 4e 00 00    	jmp    *0x4efa(%rip)        # 4060d0 <getrlimit@GLIBC_2.2.5>
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
  4011ec:	bf f0 44 40 00       	mov    $0x4044f0,%edi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:405
  4011f1:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  4011f8:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1029
  4011fd:	e8 7e fe ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1030
    fflush(stdout);
  401202:	48 8b 3d 17 4f 00 00 	mov    0x4f17(%rip),%rdi        # 406120 <stdout@GLIBC_2.2.5>
  401209:	e8 62 ff ff ff       	call   401170 <fflush@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1031
    gib_print_rust_gc_config();
  40120e:	e8 3d ff ff ff       	call   401150 <gib_print_rust_gc_config@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1032
    fflush(stdout);
  401213:	48 8b 3d 06 4f 00 00 	mov    0x4f06(%rip),%rdi        # 406120 <stdout@GLIBC_2.2.5>
  40121a:	e8 51 ff ff ff       	call   401170 <fflush@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1033
    printf("\n");
  40121f:	bf 0a 00 00 00       	mov    $0xa,%edi
  401224:	e8 17 fe ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1036


    printf("C config\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
  401229:	bf 28 45 40 00       	mov    $0x404528,%edi
  40122e:	e8 4d fe ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1040

#if defined _GIBBON_GENGC && _GIBBON_GENGC == 0
    #pragma message "Generational GC is disabled."
    printf("Generational GC is disabled.\n");
  401233:	bf 43 47 40 00       	mov    $0x404743,%edi
  401238:	e8 43 fe ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1051
#if defined _GIBBON_EAGER_PROMOTION && _GIBBON_EAGER_PROMOTION == 0
    #pragma message "Eager promotion is disabled."
    printf("Eager promotion is disabled.\n");
#else
    #pragma message "Eager promotion is enabled."
    printf("Eager promotion is enabled.\n");
  40123d:	bf 60 47 40 00       	mov    $0x404760,%edi
  401242:	e8 39 fe ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1056
#endif

#if defined _GIBBON_SIMPLE_WRITE_BARRIER && _GIBBON_SIMPLE_WRITE_BARRIER == 0
    #pragma message "Simple write barrier is disabled."
    printf("Simple write barrier is disabled.\n");
  401247:	bf 60 45 40 00       	mov    $0x404560,%edi
  40124c:	e8 2f fe ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1062
#else
    #pragma message "Simple write barrier is enabled."
    printf("Simple write barrier is enabled.\n");
#endif

    printf("Nursery size=%zu\n", (size_t) gib_nursery_size);
  401251:	be 00 00 40 00       	mov    $0x400000,%esi
  401256:	bf 7c 47 40 00       	mov    $0x40477c,%edi
  40125b:	31 c0                	xor    %eax,%eax
  40125d:	e8 5e fe ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1063
    printf("Max chunk size=%zu\n", (size_t) GIB_MAX_CHUNK_SIZE);
  401262:	be dc ff 00 00       	mov    $0xffdc,%esi
  401267:	bf 8e 47 40 00       	mov    $0x40478e,%edi
  40126c:	31 c0                	xor    %eax,%eax
  40126e:	e8 4d fe ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1064
    printf("Initial chunk size=%zu\n", (size_t) GIB_INIT_CHUNK_SIZE);
  401273:	be 00 02 00 00       	mov    $0x200,%esi
  401278:	bf a2 47 40 00       	mov    $0x4047a2,%edi
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
  40128e:	e8 2d 1c 00 00       	call   402ec0 <gib_check_rust_struct_sizes>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1909
    //   num iterations: How many times to repeat a benchmark.
    //   tree size: An integer passes to `build_tree()`.

    struct rlimit lim;
    int code;
    if ( (code = getrlimit(RLIMIT_STACK, &lim)) ) {
  401293:	48 8d 74 24 70       	lea    0x70(%rsp),%rsi
  401298:	bf 03 00 00 00       	mov    $0x3,%edi
  40129d:	e8 2e ff ff ff       	call   4011d0 <getrlimit@plt>
  4012a2:	89 44 24 08          	mov    %eax,0x8(%rsp)
  4012a6:	85 c0                	test   %eax,%eax
  4012a8:	0f 85 ce 0d 00 00    	jne    40207c <main+0xe9c>
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
  4012c5:	48 8b 3d 74 4e 00 00 	mov    0x4e74(%rip),%rdi        # 406140 <stderr@GLIBC_2.2.5>
  4012cc:	be b8 45 40 00       	mov    $0x4045b8,%esi
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
  4012eb:	0f 86 b7 02 00 00    	jbe    4015a8 <main+0x3c8>
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
  401309:	0f 8e b9 02 00 00    	jle    4015c8 <main+0x3e8>
  40130f:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  401314:	8d 6b fe             	lea    -0x2(%rbx),%ebp
  401317:	41 bd 02 00 00 00    	mov    $0x2,%r13d
  40131d:	41 bf 01 00 00 00    	mov    $0x1,%r15d
  401323:	83 e5 fe             	and    $0xfffffffe,%ebp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1968
        else if (strcmp(argv[i], "--array-input-length") == 0 && i < argc - 1) {
            check_args(i, argc, argv, "--array-input-length");
            gib_global_arrayfile_length_param = atoll(argv[i+1]);
            i++;
        }
        else if (strcmp(argv[i], "--bench-prog") == 0 && i < argc - 1) {
  401326:	44 8d 63 ff          	lea    -0x1(%rbx),%r12d
  40132a:	4c 8d 70 10          	lea    0x10(%rax),%r14
  40132e:	83 c5 03             	add    $0x3,%ebp
  401331:	eb 42                	jmp    401375 <main+0x195>
  401333:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1943
        else if (strcmp(argv[i], "--biginf-buffer-size") == 0 && i < argc - 1) {
  401338:	45 39 fc             	cmp    %r15d,%r12d
  40133b:	0f 8e cf 00 00 00    	jle    401410 <main+0x230>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  401341:	44 39 eb             	cmp    %r13d,%ebx
  401344:	0f 8e 35 0c 00 00    	jle    401f7f <main+0xd9f>
/usr/include/stdlib.h:376

# ifdef __USE_ISOC99
__extension__ __extern_inline long long int
__NTH (atoll (const char *__nptr))
{
  return strtoll (__nptr, (char **) NULL, 10);
  40134a:	49 8b 3e             	mov    (%r14),%rdi
  40134d:	ba 0a 00 00 00       	mov    $0xa,%edx
  401352:	31 f6                	xor    %esi,%esi
  401354:	e8 c7 fd ff ff       	call   401120 <strtoll@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1945
            gib_global_biginf_init_chunk_size = atoll(argv[i + 1]);
  401359:	48 89 05 90 4d 00 00 	mov    %rax,0x4d90(%rip)        # 4060f0 <gib_global_biginf_init_chunk_size>
  401360:	41 83 c7 02          	add    $0x2,%r15d
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1937
    for (i = 1; i < argc; ++i)
  401364:	41 83 c5 02          	add    $0x2,%r13d
  401368:	49 83 c6 10          	add    $0x10,%r14
  40136c:	41 39 ef             	cmp    %ebp,%r15d
  40136f:	0f 84 53 02 00 00    	je     4015c8 <main+0x3e8>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1939
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
  401375:	4d 8b 46 f8          	mov    -0x8(%r14),%r8
  401379:	41 80 38 2d          	cmpb   $0x2d,(%r8)
  40137d:	75 12                	jne    401391 <main+0x1b1>
  40137f:	41 80 78 01 68       	cmpb   $0x68,0x1(%r8)
  401384:	75 0b                	jne    401391 <main+0x1b1>
  401386:	41 80 78 02 00       	cmpb   $0x0,0x2(%r8)
  40138b:	0f 84 83 01 00 00    	je     401514 <main+0x334>
  401391:	bf ba 47 40 00       	mov    $0x4047ba,%edi
  401396:	b9 07 00 00 00       	mov    $0x7,%ecx
  40139b:	4c 89 c6             	mov    %r8,%rsi
  40139e:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  4013a0:	0f 97 c0             	seta   %al
  4013a3:	1c 00                	sbb    $0x0,%al
  4013a5:	84 c0                	test   %al,%al
  4013a7:	0f 84 67 01 00 00    	je     401514 <main+0x334>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1943
        else if (strcmp(argv[i], "--biginf-buffer-size") == 0 && i < argc - 1) {
  4013ad:	bf c1 47 40 00       	mov    $0x4047c1,%edi
  4013b2:	b9 15 00 00 00       	mov    $0x15,%ecx
  4013b7:	4c 89 c6             	mov    %r8,%rsi
  4013ba:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  4013bc:	0f 97 c0             	seta   %al
  4013bf:	1c 00                	sbb    $0x0,%al
  4013c1:	84 c0                	test   %al,%al
  4013c3:	0f 84 6f ff ff ff    	je     401338 <main+0x158>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1948
        else if (strcmp(argv[i], "--inf-buffer-size") == 0 && i < argc - 1) {
  4013c9:	bf d6 47 40 00       	mov    $0x4047d6,%edi
  4013ce:	b9 12 00 00 00       	mov    $0x12,%ecx
  4013d3:	4c 89 c6             	mov    %r8,%rsi
  4013d6:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  4013d8:	0f 97 c0             	seta   %al
  4013db:	1c 00                	sbb    $0x0,%al
  4013dd:	84 c0                	test   %al,%al
  4013df:	75 2f                	jne    401410 <main+0x230>
  4013e1:	45 39 fc             	cmp    %r15d,%r12d
  4013e4:	7e 2a                	jle    401410 <main+0x230>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  4013e6:	44 39 eb             	cmp    %r13d,%ebx
  4013e9:	0f 8e 9f 0b 00 00    	jle    401f8e <main+0xdae>
/usr/include/stdlib.h:376
  4013ef:	49 8b 3e             	mov    (%r14),%rdi
  4013f2:	ba 0a 00 00 00       	mov    $0xa,%edx
  4013f7:	31 f6                	xor    %esi,%esi
  4013f9:	e8 22 fd ff ff       	call   401120 <strtoll@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1950
            gib_global_inf_init_chunk_size = atoll(argv[i + 1]);
  4013fe:	48 89 05 e3 4c 00 00 	mov    %rax,0x4ce3(%rip)        # 4060e8 <gib_global_inf_init_chunk_size>
  401405:	e9 56 ff ff ff       	jmp    401360 <main+0x180>
  40140a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1953
        else if ((strcmp(argv[i], "--bench-input") == 0)) {
  401410:	bf e8 47 40 00       	mov    $0x4047e8,%edi
  401415:	b9 0e 00 00 00       	mov    $0xe,%ecx
  40141a:	4c 89 c6             	mov    %r8,%rsi
  40141d:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  40141f:	0f 97 c0             	seta   %al
  401422:	1c 00                	sbb    $0x0,%al
  401424:	84 c0                	test   %al,%al
  401426:	75 18                	jne    401440 <main+0x260>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  401428:	44 39 eb             	cmp    %r13d,%ebx
  40142b:	0f 8f 2f ff ff ff    	jg     401360 <main+0x180>
  401431:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  401436:	be e8 47 40 00       	mov    $0x4047e8,%esi
  40143b:	e8 b0 16 00 00       	call   402af0 <check_args.part.0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1958
        else if ((strcmp(argv[i], "--array-input") == 0)) {
  401440:	bf f6 47 40 00       	mov    $0x4047f6,%edi
  401445:	b9 0e 00 00 00       	mov    $0xe,%ecx
  40144a:	4c 89 c6             	mov    %r8,%rsi
  40144d:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  40144f:	0f 97 c0             	seta   %al
  401452:	1c 00                	sbb    $0x0,%al
  401454:	84 c0                	test   %al,%al
  401456:	75 18                	jne    401470 <main+0x290>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  401458:	44 39 eb             	cmp    %r13d,%ebx
  40145b:	0f 8f ff fe ff ff    	jg     401360 <main+0x180>
  401461:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  401466:	be f6 47 40 00       	mov    $0x4047f6,%esi
  40146b:	e8 80 16 00 00       	call   402af0 <check_args.part.0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1963
        else if (strcmp(argv[i], "--array-input-length") == 0 && i < argc - 1) {
  401470:	bf 04 48 40 00       	mov    $0x404804,%edi
  401475:	b9 15 00 00 00       	mov    $0x15,%ecx
  40147a:	4c 89 c6             	mov    %r8,%rsi
  40147d:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  40147f:	0f 97 c0             	seta   %al
  401482:	1c 00                	sbb    $0x0,%al
  401484:	84 c0                	test   %al,%al
  401486:	75 28                	jne    4014b0 <main+0x2d0>
  401488:	45 39 fc             	cmp    %r15d,%r12d
  40148b:	7e 4b                	jle    4014d8 <main+0x2f8>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  40148d:	44 39 eb             	cmp    %r13d,%ebx
  401490:	0f 8e 60 0b 00 00    	jle    401ff6 <main+0xe16>
/usr/include/stdlib.h:376
  401496:	49 8b 3e             	mov    (%r14),%rdi
  401499:	ba 0a 00 00 00       	mov    $0xa,%edx
  40149e:	31 f6                	xor    %esi,%esi
  4014a0:	e8 7b fc ff ff       	call   401120 <strtoll@plt>
  4014a5:	e9 b6 fe ff ff       	jmp    401360 <main+0x180>
  4014aa:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1968
        else if (strcmp(argv[i], "--bench-prog") == 0 && i < argc - 1) {
  4014b0:	bf 19 48 40 00       	mov    $0x404819,%edi
  4014b5:	b9 0d 00 00 00       	mov    $0xd,%ecx
  4014ba:	4c 89 c6             	mov    %r8,%rsi
  4014bd:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  4014bf:	0f 97 c0             	seta   %al
  4014c2:	1c 00                	sbb    $0x0,%al
  4014c4:	84 c0                	test   %al,%al
  4014c6:	75 10                	jne    4014d8 <main+0x2f8>
  4014c8:	45 39 fc             	cmp    %r15d,%r12d
  4014cb:	0f 8f 8d 00 00 00    	jg     40155e <main+0x37e>
  4014d1:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1975
            int len = strlen(argv[i+1]);
            gib_global_bench_prog_param = (char*) gib_alloc((len+1)*sizeof(char));
            strncpy(gib_global_bench_prog_param,argv[i+1],len);
            i++;
        }
        else if ((strcmp(argv[i], "--iterate") == 0)) {
  4014d8:	bf 26 48 40 00       	mov    $0x404826,%edi
  4014dd:	b9 0a 00 00 00       	mov    $0xa,%ecx
  4014e2:	4c 89 c6             	mov    %r8,%rsi
  4014e5:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  4014e7:	0f 97 c0             	seta   %al
  4014ea:	1c 00                	sbb    $0x0,%al
  4014ec:	84 c0                	test   %al,%al
  4014ee:	75 35                	jne    401525 <main+0x345>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  4014f0:	44 39 eb             	cmp    %r13d,%ebx
  4014f3:	0f 8e 8f 0b 00 00    	jle    402088 <main+0xea8>
/usr/include/stdlib.h:376
  4014f9:	49 8b 3e             	mov    (%r14),%rdi
  4014fc:	ba 0a 00 00 00       	mov    $0xa,%edx
  401501:	31 f6                	xor    %esi,%esi
  401503:	e8 18 fc ff ff       	call   401120 <strtoll@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1977
            check_args(i, argc, argv, "--iterate");
            gib_global_iters_param = atoll(argv[i+1]);
  401508:	48 89 05 e9 4b 00 00 	mov    %rax,0x4be9(%rip)        # 4060f8 <gib_global_iters_param>
  40150f:	e9 4c fe ff ff       	jmp    401360 <main+0x180>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1940
            gib_show_usage(argv);
  401514:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  401519:	e8 12 15 00 00       	call   402a30 <gib_show_usage>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1941
            exit(0);
  40151e:	31 ff                	xor    %edi,%edi
  401520:	e8 8b fc ff ff       	call   4011b0 <exit@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1980
            i++;
        }
        else if ((strcmp(argv[i], "--size-param") == 0)) {
  401525:	be 30 48 40 00       	mov    $0x404830,%esi
  40152a:	4c 89 c7             	mov    %r8,%rdi
  40152d:	e8 de fb ff ff       	call   401110 <strcmp@plt>
  401532:	85 c0                	test   %eax,%eax
  401534:	0f 85 da 0a 00 00    	jne    402014 <main+0xe34>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  40153a:	44 39 eb             	cmp    %r13d,%ebx
  40153d:	0f 8e c2 0a 00 00    	jle    402005 <main+0xe25>
/usr/include/stdlib.h:376
  401543:	49 8b 3e             	mov    (%r14),%rdi
  401546:	ba 0a 00 00 00       	mov    $0xa,%edx
  40154b:	31 f6                	xor    %esi,%esi
  40154d:	e8 ce fb ff ff       	call   401120 <strtoll@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1982
            check_args(i, argc, argv, "--size-param");
            gib_global_size_param = atoll(argv[i+1]);
  401552:	48 89 05 a7 4b 00 00 	mov    %rax,0x4ba7(%rip)        # 406100 <gib_global_size_param>
  401559:	e9 02 fe ff ff       	jmp    401360 <main+0x180>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  40155e:	44 39 eb             	cmp    %r13d,%ebx
  401561:	0f 8e 06 0b 00 00    	jle    40206d <main+0xe8d>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1970
            int len = strlen(argv[i+1]);
  401567:	49 8b 36             	mov    (%r14),%rsi
  40156a:	48 89 f7             	mov    %rsi,%rdi
  40156d:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  401572:	e8 39 fb ff ff       	call   4010b0 <strlen@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1971
            gib_global_bench_prog_param = (char*) gib_alloc((len+1)*sizeof(char));
  401577:	8d 78 01             	lea    0x1(%rax),%edi
  40157a:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40157f:	48 63 ff             	movslq %edi,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401582:	e8 d9 fb ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1972
            strncpy(gib_global_bench_prog_param,argv[i+1],len);
  401587:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40158c:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401591:	48 89 c7             	mov    %rax,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1971
            gib_global_bench_prog_param = (char*) gib_alloc((len+1)*sizeof(char));
  401594:	48 89 05 ed 4b 00 00 	mov    %rax,0x4bed(%rip)        # 406188 <gib_global_bench_prog_param>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1972
            strncpy(gib_global_bench_prog_param,argv[i+1],len);
  40159b:	48 63 d2             	movslq %edx,%rdx
  40159e:	e8 ad fa ff ff       	call   401050 <strncpy@plt>
  4015a3:	e9 b8 fd ff ff       	jmp    401360 <main+0x180>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1927
            fprintf(stderr, " [gibbon rts] Failed setrlimit stack size to something reasonable; giving up.\n");
  4015a8:	ba 4e 00 00 00       	mov    $0x4e,%edx
  4015ad:	be 01 00 00 00       	mov    $0x1,%esi
  4015b2:	bf f0 45 40 00       	mov    $0x4045f0,%edi
  4015b7:	48 8b 0d 82 4b 00 00 	mov    0x4b82(%rip),%rcx        # 406140 <stderr@GLIBC_2.2.5>
  4015be:	e8 fd fb ff ff       	call   4011c0 <fwrite@plt>
  4015c3:	e9 3e fd ff ff       	jmp    401306 <main+0x126>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1996
        }
    }

    // Initialize gib_global_bench_prog_param to an empty string in case
    // the runtime argument --bench-prog isn't passed.
    if (gib_global_bench_prog_param == NULL) {
  4015c8:	48 83 3d b8 4b 00 00 	cmpq   $0x0,0x4bb8(%rip)        # 406188 <gib_global_bench_prog_param>
  4015cf:	00 
  4015d0:	0f 84 45 09 00 00    	je     401f1b <main+0xd3b>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:2007
#ifdef _GIBBON_PARALLEL
    gib_global_num_threads = __cilkrts_get_nworkers();
#endif

#if defined _GIBBON_VERBOSITY && _GIBBON_VERBOSITY >= 2
    printf("Number of threads: %ld\n", gib_global_num_threads);
  4015d6:	be 01 00 00 00       	mov    $0x1,%esi
  4015db:	bf 5d 48 40 00       	mov    $0x40485d,%edi
  4015e0:	31 c0                	xor    %eax,%eax
  4015e2:	e8 d9 fa ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4015e7:	bf f0 00 00 00       	mov    $0xf0,%edi
  4015ec:	e8 6f fb ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1565
    stats->minor_collections = 0;
  4015f1:	66 0f ef c0          	pxor   %xmm0,%xmm0
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1581
    stats->gc_elapsed_time = 0;
  4015f5:	66 0f ef c9          	pxor   %xmm1,%xmm1
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4015f9:	bf 20 00 00 00       	mov    $0x20,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1587
    stats->gc_zct_mgmt_time = 0;
  4015fe:	48 c7 80 b0 00 00 00 	movq   $0x0,0xb0(%rax)
  401605:	00 00 00 00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1594
    stats->rootset_size = 0;
  401609:	48 c7 80 e8 00 00 00 	movq   $0x0,0xe8(%rax)
  401610:	00 00 00 00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1565
    stats->minor_collections = 0;
  401614:	0f 11 00             	movups %xmm0,(%rax)
  401617:	0f 11 40 10          	movups %xmm0,0x10(%rax)
  40161b:	0f 11 40 20          	movups %xmm0,0x20(%rax)
  40161f:	0f 11 40 30          	movups %xmm0,0x30(%rax)
  401623:	0f 11 40 40          	movups %xmm0,0x40(%rax)
  401627:	0f 11 40 50          	movups %xmm0,0x50(%rax)
  40162b:	0f 11 40 60          	movups %xmm0,0x60(%rax)
  40162f:	0f 11 40 70          	movups %xmm0,0x70(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1581
    stats->gc_elapsed_time = 0;
  401633:	0f 11 88 80 00 00 00 	movups %xmm1,0x80(%rax)
  40163a:	0f 11 88 90 00 00 00 	movups %xmm1,0x90(%rax)
  401641:	0f 11 88 a0 00 00 00 	movups %xmm1,0xa0(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1588
    stats->fwd_env_size = 0;
  401648:	0f 11 80 b8 00 00 00 	movups %xmm0,0xb8(%rax)
  40164f:	0f 11 80 c8 00 00 00 	movups %xmm0,0xc8(%rax)
  401656:	0f 11 80 d8 00 00 00 	movups %xmm0,0xd8(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1338
    gib_global_gc_stats = (GibGcStats *) gib_alloc(sizeof(GibGcStats));
  40165d:	48 89 05 fc 4a 00 00 	mov    %rax,0x4afc(%rip)        # 406160 <gib_global_gc_stats>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401664:	e8 f7 fa ff ff       	call   401160 <malloc@plt>
  401669:	bf 00 00 40 00       	mov    $0x400000,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1420
    nursery->heap_size = nsize;
  40166e:	48 c7 00 00 00 40 00 	movq   $0x400000,(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401675:	48 89 c3             	mov    %rax,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1343
    gib_global_nurseries = (GibNursery *) gib_alloc(gib_global_num_threads *
  401678:	48 89 05 01 4b 00 00 	mov    %rax,0x4b01(%rip)        # 406180 <gib_global_nurseries>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  40167f:	e8 dc fa ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1421
    nursery->heap_start = (char *) gib_alloc(nsize);
  401684:	48 89 43 08          	mov    %rax,0x8(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401688:	48 89 c6             	mov    %rax,%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1422
    if (nursery->heap_start == NULL) {
  40168b:	48 85 c0             	test   %rax,%rax
  40168e:	0f 84 56 09 00 00    	je     401fea <main+0xe0a>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1427
    nursery->heap_end = nursery->heap_start + nsize;
  401694:	48 8d 90 00 00 40 00 	lea    0x400000(%rax),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1431
    printf("Nursery info: start=%p, end=%p, alloc=%p, size=%zu\n\n",
  40169b:	41 b8 00 00 40 00    	mov    $0x400000,%r8d
  4016a1:	bf 70 46 40 00       	mov    $0x404670,%edi
  4016a6:	31 c0                	xor    %eax,%eax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1427
    nursery->heap_end = nursery->heap_start + nsize;
  4016a8:	48 89 53 10          	mov    %rdx,0x10(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1431
    printf("Nursery info: start=%p, end=%p, alloc=%p, size=%zu\n\n",
  4016ac:	48 89 d1             	mov    %rdx,%rcx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1428
    nursery->alloc = nursery->heap_end;
  4016af:	48 89 53 18          	mov    %rdx,0x18(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1431
    printf("Nursery info: start=%p, end=%p, alloc=%p, size=%zu\n\n",
  4016b3:	e8 08 fa ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4016b8:	bf 18 00 00 00       	mov    $0x18,%edi
  4016bd:	e8 9e fa ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1454
    oldgen->old_zct = (void *) NULL;
  4016c2:	66 0f ef c0          	pxor   %xmm0,%xmm0
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4016c6:	bf 18 00 00 00       	mov    $0x18,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1454
    oldgen->old_zct = (void *) NULL;
  4016cb:	0f 11 40 08          	movups %xmm0,0x8(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4016cf:	48 89 c5             	mov    %rax,%rbp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1350
    gib_global_oldgen = (GibOldgen *) gib_alloc(sizeof(GibOldgen));
  4016d2:	48 89 05 8f 4a 00 00 	mov    %rax,0x4a8f(%rip)        # 406168 <gib_global_oldgen>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4016d9:	e8 82 fa ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1457
    oldgen->rem_set = (GibRememberedSet *) gib_alloc(sizeof(GibRememberedSet));
  4016de:	48 89 45 00          	mov    %rax,0x0(%rbp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4016e2:	48 89 c3             	mov    %rax,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1458
    if (oldgen->rem_set == NULL) {
  4016e5:	48 85 c0             	test   %rax,%rax
  4016e8:	0f 84 f0 08 00 00    	je     401fde <main+0xdfe>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4016ee:	bf 00 00 00 06       	mov    $0x6000000,%edi
  4016f3:	e8 68 fa ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1485
    stack->start = (char *) gib_alloc(stack_size);
  4016f8:	48 89 03             	mov    %rax,(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1486
    if (stack->start == NULL) {
  4016fb:	48 85 c0             	test   %rax,%rax
  4016fe:	0f 84 56 08 00 00    	je     401f5a <main+0xd7a>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1491
    stack->end = stack->start + stack_size;
  401704:	48 8d 90 00 00 00 06 	lea    0x6000000(%rax),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1492
    stack->alloc = stack->start;
  40170b:	48 89 43 10          	mov    %rax,0x10(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  40170f:	bf 18 00 00 00       	mov    $0x18,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1491
    stack->end = stack->start + stack_size;
  401714:	48 89 53 08          	mov    %rdx,0x8(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401718:	e8 43 fa ff ff       	call   401160 <malloc@plt>
  40171d:	bf 18 00 00 00       	mov    $0x18,%edi
  401722:	49 89 c4             	mov    %rax,%r12
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1355
    gib_global_read_shadowstacks =
  401725:	48 89 05 4c 4a 00 00 	mov    %rax,0x4a4c(%rip)        # 406178 <gib_global_read_shadowstacks>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  40172c:	e8 2f fa ff ff       	call   401160 <malloc@plt>
  401731:	bf 00 00 00 06       	mov    $0x6000000,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1358
    gib_global_write_shadowstacks =
  401736:	48 89 05 33 4a 00 00 	mov    %rax,0x4a33(%rip)        # 406170 <gib_global_write_shadowstacks>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  40173d:	48 89 c3             	mov    %rax,%rbx
  401740:	e8 1b fa ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1485
    stack->start = (char *) gib_alloc(stack_size);
  401745:	49 89 04 24          	mov    %rax,(%r12)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1486
    if (stack->start == NULL) {
  401749:	48 85 c0             	test   %rax,%rax
  40174c:	0f 84 08 08 00 00    	je     401f5a <main+0xd7a>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1491
    stack->end = stack->start + stack_size;
  401752:	48 8d 90 00 00 00 06 	lea    0x6000000(%rax),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1492
    stack->alloc = stack->start;
  401759:	49 89 44 24 10       	mov    %rax,0x10(%r12)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  40175e:	bf 00 00 00 06       	mov    $0x6000000,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1491
    stack->end = stack->start + stack_size;
  401763:	49 89 54 24 08       	mov    %rdx,0x8(%r12)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401768:	e8 f3 f9 ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1485
    stack->start = (char *) gib_alloc(stack_size);
  40176d:	48 89 03             	mov    %rax,(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1486
    if (stack->start == NULL) {
  401770:	48 85 c0             	test   %rax,%rax
  401773:	0f 84 e1 07 00 00    	je     401f5a <main+0xd7a>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1491
    stack->end = stack->start + stack_size;
  401779:	48 8d 90 00 00 00 06 	lea    0x6000000(%rax),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1492
    stack->alloc = stack->start;
  401780:	48 89 43 10          	mov    %rax,0x10(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:2014

#ifndef _GIBBON_POINTER
    // Initialize the nursery and shadow stack.
    gib_storage_initialize();
    GibOldgen *oldgen = DEFAULT_GENERATION;
    gib_init_zcts(oldgen);
  401784:	48 89 ef             	mov    %rbp,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1491
    stack->end = stack->start + stack_size;
  401787:	48 89 53 08          	mov    %rdx,0x8(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:2014
    gib_init_zcts(oldgen);
  40178b:	e8 e0 f8 ff ff       	call   401070 <gib_init_zcts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:2017

    // Minimal test to see if FFI is set up correctly.
    gib_check_rust_struct_sizes();
  401790:	e8 2b 17 00 00       	call   402ec0 <gib_check_rust_struct_sizes>
info_table_initialize():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:87
    int error = gib_info_table_initialize(7);
  401795:	bf 07 00 00 00       	mov    $0x7,%edi
  40179a:	e8 51 f9 ff ff       	call   4010f0 <gib_info_table_initialize@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:89
    if (error < 0) {
  40179f:	85 c0                	test   %eax,%eax
  4017a1:	0f 88 18 08 00 00    	js     401fbf <main+0xddf>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:96
    gib_info_table_finalize();
  4017a7:	e8 f4 f9 ff ff       	call   4011a0 <gib_info_table_finalize@plt>
symbol_table_initialize():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:100
    gib_add_symbol(1236, "]");
  4017ac:	be 19 47 40 00       	mov    $0x404719,%esi
  4017b1:	bf d4 04 00 00       	mov    $0x4d4,%edi
  4017b6:	41 bc 00 00 00 00    	mov    $0x0,%r12d
  4017bc:	e8 5f 13 00 00       	call   402b20 <gib_add_symbol>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:101
    gib_add_symbol(1237, "[");
  4017c1:	be 0f 47 40 00       	mov    $0x40470f,%esi
  4017c6:	bf d5 04 00 00       	mov    $0x4d5,%edi
  4017cb:	e8 50 13 00 00       	call   402b20 <gib_add_symbol>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:102
    gib_add_symbol(1238, ",");
  4017d0:	be 75 48 40 00       	mov    $0x404875,%esi
  4017d5:	bf d6 04 00 00       	mov    $0x4d6,%edi
  4017da:	e8 41 13 00 00       	call   402b20 <gib_add_symbol>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:78
    return gib_global_size_param;
  4017df:	4c 8b 2d 1a 49 00 00 	mov    0x491a(%rip),%r13        # 406100 <gib_global_size_param>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:108
    if (fltIf_1019_1066) {
  4017e6:	4d 85 ed             	test   %r13,%r13
  4017e9:	4d 0f 49 e5          	cmovns %r13,%r12
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:415
    
    GibInt n_42_859_1054 = gib_get_size_param();
    GibInt n_100_892_961_1055 = gib_get_size_param();
    GibInt n__103_894_963_1057 =  maxInt(n_100_892_961_1055, 0);
    GibInt tmp_19 = sizeof(GibInt);
    GibVector *vec_104_895_964_1058 = gib_vector_alloc(n__103_894_963_1057,
  4017ed:	4c 89 e7             	mov    %r12,%rdi
  4017f0:	e8 cb 10 00 00       	call   4028c0 <gib_vector_alloc.constprop.0>
  4017f5:	48 89 c5             	mov    %rax,%rbp
generate_loop_544_803():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:154
    if (fltIf_1023_1091) {
  4017f8:	4d 85 ed             	test   %r13,%r13
  4017fb:	7e 13                	jle    401810 <main+0x630>
  4017fd:	4c 89 e9             	mov    %r13,%rcx
  401800:	4c 89 e2             	mov    %r12,%rdx
  401803:	31 f6                	xor    %esi,%esi
  401805:	48 89 c7             	mov    %rax,%rdi
  401808:	e8 63 0b 00 00       	call   402370 <generate_loop_544_803.part.0>
  40180d:	48 89 c5             	mov    %rax,%rbp
main():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:420
                                                       tmp_19);
    GibVector *vec1_105_896_965_1059 =
               generate_loop_544_803(vec_104_895_964_1058, 0, n__103_894_963_1057, n_42_859_1054);
    GibVector *timed_1232;
    GibVector *times_12 = gib_vector_alloc(gib_get_iters_param(),
  401810:	48 8b 3d e1 48 00 00 	mov    0x48e1(%rip),%rdi        # 4060f8 <gib_global_iters_param>
  401817:	e8 a4 10 00 00       	call   4028c0 <gib_vector_alloc.constprop.0>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:425
                                           sizeof(double));
    struct timespec begin_timed_1232;
    struct timespec end_timed_1232;
    
    for (long long iters_timed_1232 = 0; iters_timed_1232 <
  40181c:	48 83 3d d4 48 00 00 	cmpq   $0x0,0x48d4(%rip)        # 4060f8 <gib_global_iters_param>
  401823:	00 
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:420
    GibVector *times_12 = gib_vector_alloc(gib_get_iters_param(),
  401824:	49 89 c4             	mov    %rax,%r12
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:425
    for (long long iters_timed_1232 = 0; iters_timed_1232 <
  401827:	0f 8e dc 01 00 00    	jle    401a09 <main+0x829>
  40182d:	45 31 ed             	xor    %r13d,%r13d
  401830:	e9 ad 00 00 00       	jmp    4018e2 <main+0x702>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401835:	48 8b 45 00          	mov    0x0(%rbp),%rax
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:254
            GibInt fltAppE_1037_1143 = *tmp_4;
  401839:	48 8b 55 18          	mov    0x18(%rbp),%rdx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:256
                       insert_541_809(xs__66_943_1140, fltAppE_1037_1143, n_63_941_1134);
  40183d:	4c 89 f7             	mov    %r14,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  401840:	4c 01 f8             	add    %r15,%rax
  401843:	48 0f af 45 10       	imul   0x10(%rbp),%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:256
  401848:	48 8b 34 02          	mov    (%rdx,%rax,1),%rsi
  40184c:	4c 89 fa             	mov    %r15,%rdx
  40184f:	e8 5c 0a 00 00       	call   4022b0 <insert_541_809>
  401854:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
main():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:436
        clock_gettime(CLOCK_MONOTONIC_RAW, &begin_timed_1232);
        
        GibVector *tailapp_1229 =  isort1_533_796(vec1_105_896_965_1059);
        
        timed_1232 = tailapp_1229;
        clock_gettime(CLOCK_MONOTONIC_RAW, &end_timed_1232);
  401859:	48 8d 74 24 60       	lea    0x60(%rsp),%rsi
  40185e:	bf 04 00 00 00       	mov    $0x4,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401863:	4c 8d 7c 24 50       	lea    0x50(%rsp),%r15
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:436
  401868:	e8 33 f8 ff ff       	call   4010a0 <clock_gettime@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1828
            + ((double)(t1->tv_nsec - t0->tv_nsec) / 1000000000.0);
  40186d:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401872:	66 0f ef c0          	pxor   %xmm0,%xmm0
  401876:	48 2b 44 24 78       	sub    0x78(%rsp),%rax
  40187b:	f2 48 0f 2a c0       	cvtsi2sd %rax,%xmm0
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1827
    return (double)(t1->tv_sec - t0->tv_sec)
  401880:	66 0f ef c9          	pxor   %xmm1,%xmm1
  401884:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401889:	48 2b 44 24 70       	sub    0x70(%rsp),%rax
  40188e:	f2 48 0f 2a c8       	cvtsi2sd %rax,%xmm1
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:445
        }
        
        double itertime_9 = gib_difftimespecs(&begin_timed_1232,
                                              &end_timed_1232);
        
        printf("itertime: %lf\n", itertime_9);
  401893:	bf 77 48 40 00       	mov    $0x404877,%edi
  401898:	b8 01 00 00 00       	mov    $0x1,%eax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1828
            + ((double)(t1->tv_nsec - t0->tv_nsec) / 1000000000.0);
  40189d:	f2 0f 5e 05 53 30 00 	divsd  0x3053(%rip),%xmm0        # 4048f8 <__PRETTY_FUNCTION__.3+0x28>
  4018a4:	00 
  4018a5:	f2 0f 58 c1          	addsd  %xmm1,%xmm0
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:442
        double itertime_9 = gib_difftimespecs(&begin_timed_1232,
  4018a9:	f2 0f 11 44 24 50    	movsd  %xmm0,0x50(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:445
        printf("itertime: %lf\n", itertime_9);
  4018af:	e8 0c f8 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4018b4:	49 8b 3c 24          	mov    (%r12),%rdi
  4018b8:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4018bd:	4c 89 fe             	mov    %r15,%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4018c0:	4c 01 ef             	add    %r13,%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:426
         gib_get_iters_param(); iters_timed_1232++) {
  4018c3:	49 83 c5 01          	add    $0x1,%r13
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4018c7:	48 0f af fa          	imul   %rdx,%rdi
  4018cb:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4018d0:	e8 6b f8 ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:425
    for (long long iters_timed_1232 = 0; iters_timed_1232 <
  4018d5:	4c 3b 2d 1c 48 00 00 	cmp    0x481c(%rip),%r13        # 4060f8 <gib_global_iters_param>
  4018dc:	0f 8d 27 01 00 00    	jge    401a09 <main+0x829>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:431
        clock_gettime(CLOCK_MONOTONIC_RAW, &begin_timed_1232);
  4018e2:	48 8d 74 24 70       	lea    0x70(%rsp),%rsi
  4018e7:	bf 04 00 00 00       	mov    $0x4,%edi
  4018ec:	41 bf 00 00 00 00    	mov    $0x0,%r15d
  4018f2:	e8 a9 f7 ff ff       	call   4010a0 <clock_gettime@plt>
isort1_533_796():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:195
    GibInt n_56_931_1109 = gib_vector_length(xs_54_930_1107);
  4018f7:	48 8b 45 00          	mov    0x0(%rbp),%rax
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4018fb:	48 8b 55 10          	mov    0x10(%rbp),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4018ff:	bf 20 00 00 00       	mov    $0x20,%edi
isort1_533_796():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:200
    GibInt hd_57_932_1112 = *tmp_3;
  401904:	48 8b 4d 18          	mov    0x18(%rbp),%rcx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401908:	48 0f af d0          	imul   %rax,%rdx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:200
  40190c:	48 8b 1c 11          	mov    (%rcx,%rdx,1),%rbx
  401910:	48 89 5c 24 10       	mov    %rbx,0x10(%rsp)
main():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:108
    if (fltIf_1019_1066) {
  401915:	48 8b 5d 08          	mov    0x8(%rbp),%rbx
  401919:	48 29 c3             	sub    %rax,%rbx
  40191c:	4c 0f 49 fb          	cmovns %rbx,%r15
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401920:	e8 3b f8 ff ff       	call   401160 <malloc@plt>
  401925:	49 89 c6             	mov    %rax,%r14
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:515
    if (vec == NULL) {
  401928:	48 85 c0             	test   %rax,%rax
  40192b:	0f 84 6c 06 00 00    	je     401f9d <main+0xdbd>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:519
    void *data = (void *) gib_alloc(num * elt_size);
  401931:	4a 8d 3c fd 00 00 00 	lea    0x0(,%r15,8),%rdi
  401938:	00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401939:	e8 22 f8 ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:520
    if (data == NULL) {
  40193e:	48 85 c0             	test   %rax,%rax
  401941:	0f 84 18 06 00 00    	je     401f5f <main+0xd7f>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:524
    vec->lower = 0;
  401947:	49 c7 06 00 00 00 00 	movq   $0x0,(%r14)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:525
    vec->upper = num;
  40194e:	4d 89 7e 08          	mov    %r15,0x8(%r14)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:526
    vec->elt_size = elt_size;
  401952:	49 c7 46 10 08 00 00 	movq   $0x8,0x10(%r14)
  401959:	00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:527
    vec->data = data;
  40195a:	49 89 46 18          	mov    %rax,0x18(%r14)
generate_loop_544_808():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:220
    if (fltIf_1031_1126) {
  40195e:	48 85 db             	test   %rbx,%rbx
  401961:	7e 4d                	jle    4019b0 <main+0x7d0>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401963:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  401968:	48 89 10             	mov    %rdx,(%rax)
generate_loop_544_808():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:220
  40196b:	48 83 fb 01          	cmp    $0x1,%rbx
  40196f:	74 3f                	je     4019b0 <main+0x7d0>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
  401971:	48 89 50 08          	mov    %rdx,0x8(%rax)
generate_loop_544_808():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:220
  401975:	48 83 fb 02          	cmp    $0x2,%rbx
  401979:	74 35                	je     4019b0 <main+0x7d0>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
  40197b:	48 89 50 10          	mov    %rdx,0x10(%rax)
generate_loop_544_808():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:220
  40197f:	48 83 fb 03          	cmp    $0x3,%rbx
  401983:	74 2b                	je     4019b0 <main+0x7d0>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
  401985:	48 89 50 18          	mov    %rdx,0x18(%rax)
generate_loop_544_808():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:220
  401989:	48 83 fb 04          	cmp    $0x4,%rbx
  40198d:	74 21                	je     4019b0 <main+0x7d0>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
  40198f:	48 89 50 20          	mov    %rdx,0x20(%rax)
generate_loop_544_808():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:228
                   generate_loop_544_808(vec1_259_938_1130, fltAppE_1033_1131, end_256_936_1124, hd_57_937_1125);
  401993:	48 89 d1             	mov    %rdx,%rcx
  401996:	4c 89 f7             	mov    %r14,%rdi
  401999:	4c 89 fa             	mov    %r15,%rdx
  40199c:	be 05 00 00 00       	mov    $0x5,%esi
  4019a1:	e8 0a 08 00 00       	call   4021b0 <generate_loop_544_808>
  4019a6:	49 89 c6             	mov    %rax,%r14
  4019a9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:533
    return (vec->upper - vec->lower);
  4019b0:	48 8b 55 08          	mov    0x8(%rbp),%rdx
  4019b4:	48 2b 55 00          	sub    0x0(%rbp),%rdx
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:239
    if (fltIf_1034_1137) {
  4019b8:	48 89 6c 24 18       	mov    %rbp,0x18(%rsp)
  4019bd:	48 83 fa 01          	cmp    $0x1,%rdx
  4019c1:	0f 8e 92 fe ff ff    	jle    401859 <main+0x679>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:244
        if (fltIf_1035_1138) {
  4019c7:	48 89 d3             	mov    %rdx,%rbx
isort1_533_796():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:208
    GibInt fltAppE_1029_1121 = fltPrm_1030_1120 - 1;
  4019ca:	4c 8d 7a ff          	lea    -0x1(%rdx),%r15
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:244
        if (fltIf_1035_1138) {
  4019ce:	48 83 eb 02          	sub    $0x2,%rbx
  4019d2:	0f 84 5d fe ff ff    	je     401835 <main+0x655>
  4019d8:	48 83 ea 03          	sub    $0x3,%rdx
  4019dc:	0f 85 52 05 00 00    	jne    401f34 <main+0xd54>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4019e2:	48 8b 45 00          	mov    0x0(%rbp),%rax
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:254
            GibInt fltAppE_1037_1143 = *tmp_4;
  4019e6:	48 8b 55 18          	mov    0x18(%rbp),%rdx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:256
                       insert_541_809(xs__66_943_1140, fltAppE_1037_1143, n_63_941_1134);
  4019ea:	4c 89 f7             	mov    %r14,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4019ed:	48 01 d8             	add    %rbx,%rax
  4019f0:	48 0f af 45 10       	imul   0x10(%rbp),%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:256
  4019f5:	48 8b 34 02          	mov    (%rdx,%rax,1),%rsi
  4019f9:	48 89 da             	mov    %rbx,%rdx
  4019fc:	e8 af 08 00 00       	call   4022b0 <insert_541_809>
  401a01:	49 89 c6             	mov    %rax,%r14
  401a04:	e9 2c fe ff ff       	jmp    401835 <main+0x655>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  401a09:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
  401a0e:	49 8b 3c 24          	mov    (%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:599
    qsort(start, gib_vector_length(vec), vec->elt_size, compar);
  401a12:	b9 90 21 40 00       	mov    $0x402190,%ecx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:533
    return (vec->upper - vec->lower);
  401a17:	49 8b 74 24 08       	mov    0x8(%r12),%rsi
  401a1c:	48 29 fe             	sub    %rdi,%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401a1f:	48 0f af fa          	imul   %rdx,%rdi
  401a23:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:599
    qsort(start, gib_vector_length(vec), vec->elt_size, compar);
  401a28:	e8 63 f6 ff ff       	call   401090 <qsort@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:450
        gib_vector_inplace_update(times_12, iters_timed_1232, &itertime_9);
    }
    gib_vector_inplace_sort(times_12, gib_compare_doubles);
    
    double *tmp_13 = (double *) gib_vector_nth(times_12, gib_get_iters_param() /
  401a2d:	48 8b 05 c4 46 00 00 	mov    0x46c4(%rip),%rax        # 4060f8 <gib_global_iters_param>
  401a34:	41 b8 02 00 00 00    	mov    $0x2,%r8d
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401a3a:	49 8b 3c 24          	mov    (%r12),%rdi
  401a3e:	49 8b 74 24 10       	mov    0x10(%r12),%rsi
  401a43:	49 8b 4c 24 18       	mov    0x18(%r12),%rcx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:450
  401a48:	48 99                	cqto
  401a4a:	49 f7 f8             	idiv   %r8
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:533
    return (vec->upper - vec->lower);
  401a4d:	49 8b 54 24 08       	mov    0x8(%r12),%rdx
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
  401a64:	0f 8e dd 04 00 00    	jle    401f47 <main+0xd67>
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
  401a96:	4c 89 e7             	mov    %r12,%rdi
  401a99:	f2 0f 11 4c 24 10    	movsd  %xmm1,0x10(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:468
    GibVector *times_17 = gib_vector_alloc(gib_get_iters_param(),
                                           sizeof(double));
    struct timespec begin_timed_1233;
    struct timespec end_timed_1233;
    
    for (long long iters_timed_1233 = 0; iters_timed_1233 <
  401a9f:	31 db                	xor    %ebx,%ebx
  401aa1:	4c 8d 7c 24 50       	lea    0x50(%rsp),%r15
  401aa6:	f2 0f 11 44 24 28    	movsd  %xmm0,0x28(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:455
    gib_print_timing_array(times_12);
  401aac:	e8 df 0e 00 00       	call   402990 <gib_print_timing_array>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:242
void gib_free(void *ptr) { free(ptr); }
  401ab1:	49 8b 7c 24 18       	mov    0x18(%r12),%rdi
  401ab6:	e8 75 f5 ff ff       	call   401030 <free@plt>
  401abb:	4c 89 e7             	mov    %r12,%rdi
  401abe:	e8 6d f5 ff ff       	call   401030 <free@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:457
    printf("ITERS: %ld\n", gib_get_iters_param());
  401ac3:	48 8b 35 2e 46 00 00 	mov    0x462e(%rip),%rsi        # 4060f8 <gib_global_iters_param>
  401aca:	bf 86 48 40 00       	mov    $0x404886,%edi
  401acf:	31 c0                	xor    %eax,%eax
  401ad1:	e8 ea f5 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:458
    printf("SIZE: %ld\n", gib_get_size_param());
  401ad6:	48 8b 35 23 46 00 00 	mov    0x4623(%rip),%rsi        # 406100 <gib_global_size_param>
  401add:	bf 92 48 40 00       	mov    $0x404892,%edi
  401ae2:	31 c0                	xor    %eax,%eax
  401ae4:	e8 d7 f5 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:459
    printf("BATCHTIME: %e\n", batchtime_10);
  401ae9:	f2 0f 10 44 24 28    	movsd  0x28(%rsp),%xmm0
  401aef:	bf 9d 48 40 00       	mov    $0x40489d,%edi
  401af4:	b8 01 00 00 00       	mov    $0x1,%eax
  401af9:	e8 c2 f5 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:460
    printf("SELFTIMED: %e\n", selftimed_11);
  401afe:	f2 0f 10 4c 24 10    	movsd  0x10(%rsp),%xmm1
  401b04:	bf ac 48 40 00       	mov    $0x4048ac,%edi
  401b09:	b8 01 00 00 00       	mov    $0x1,%eax
  401b0e:	66 0f 28 c1          	movapd %xmm1,%xmm0
  401b12:	e8 a9 f5 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:463
    GibVector *times_17 = gib_vector_alloc(gib_get_iters_param(),
  401b17:	48 8b 3d da 45 00 00 	mov    0x45da(%rip),%rdi        # 4060f8 <gib_global_iters_param>
  401b1e:	e8 9d 0d 00 00       	call   4028c0 <gib_vector_alloc.constprop.0>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:468
    for (long long iters_timed_1233 = 0; iters_timed_1233 <
  401b23:	48 83 3d cd 45 00 00 	cmpq   $0x0,0x45cd(%rip)        # 4060f8 <gib_global_iters_param>
  401b2a:	00 
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:463
    GibVector *times_17 = gib_vector_alloc(gib_get_iters_param(),
  401b2b:	49 89 c5             	mov    %rax,%r13
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:468
    for (long long iters_timed_1233 = 0; iters_timed_1233 <
  401b2e:	0f 8f f8 00 00 00    	jg     401c2c <main+0xa4c>
  401b34:	e9 2d 02 00 00       	jmp    401d66 <main+0xb86>
  401b39:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:331
GibVector *go_538_810(GibInt i_82_951_1174, GibInt n_83_952_1175,
  401b40:	4c 89 f6             	mov    %r14,%rsi
  401b43:	bf 01 00 00 00       	mov    $0x1,%edi
  401b48:	e8 c3 0b 00 00       	call   402710 <shift_543_811.part.0>
  401b4d:	bf 02 00 00 00       	mov    $0x2,%edi
  401b52:	48 89 c6             	mov    %rax,%rsi
  401b55:	e8 b6 0b 00 00       	call   402710 <shift_543_811.part.0>
  401b5a:	48 89 c6             	mov    %rax,%rsi
  401b5d:	bf 03 00 00 00       	mov    $0x3,%edi
  401b62:	e8 a9 0b 00 00       	call   402710 <shift_543_811.part.0>
  401b67:	48 89 c6             	mov    %rax,%rsi
go_538_810():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
    if (fltIf_1046_1177) {
  401b6a:	49 83 fc 04          	cmp    $0x4,%r12
  401b6e:	74 38                	je     401ba8 <main+0x9c8>
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:331
GibVector *go_538_810(GibInt i_82_951_1174, GibInt n_83_952_1175,
  401b70:	bf 04 00 00 00       	mov    $0x4,%edi
  401b75:	e8 96 0b 00 00       	call   402710 <shift_543_811.part.0>
  401b7a:	48 89 c6             	mov    %rax,%rsi
go_538_810():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
    if (fltIf_1046_1177) {
  401b7d:	49 83 fc 05          	cmp    $0x5,%r12
  401b81:	74 25                	je     401ba8 <main+0x9c8>
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:331
GibVector *go_538_810(GibInt i_82_951_1174, GibInt n_83_952_1175,
  401b83:	bf 05 00 00 00       	mov    $0x5,%edi
  401b88:	e8 83 0b 00 00       	call   402710 <shift_543_811.part.0>
  401b8d:	48 89 c2             	mov    %rax,%rdx
go_538_810():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
    if (fltIf_1046_1177) {
  401b90:	49 83 fc 06          	cmp    $0x6,%r12
  401b94:	74 12                	je     401ba8 <main+0x9c8>
  401b96:	4c 89 e6             	mov    %r12,%rsi
  401b99:	bf 06 00 00 00       	mov    $0x6,%edi
  401b9e:	e8 0d 0c 00 00       	call   4027b0 <go_538_810.part.0.isra.0>
  401ba3:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
main():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:479
        clock_gettime(CLOCK_MONOTONIC_RAW, &begin_timed_1233);
        
        GibVector *tailapp_1230 =  isort2_534_797(vec1_105_896_965_1059);
        
        timed_1233 = tailapp_1230;
        clock_gettime(CLOCK_MONOTONIC_RAW, &end_timed_1233);
  401ba8:	48 8d 74 24 40       	lea    0x40(%rsp),%rsi
  401bad:	bf 04 00 00 00       	mov    $0x4,%edi
  401bb2:	e8 e9 f4 ff ff       	call   4010a0 <clock_gettime@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1828
            + ((double)(t1->tv_nsec - t0->tv_nsec) / 1000000000.0);
  401bb7:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401bbc:	66 0f ef c0          	pxor   %xmm0,%xmm0
  401bc0:	48 2b 44 24 58       	sub    0x58(%rsp),%rax
  401bc5:	f2 48 0f 2a c0       	cvtsi2sd %rax,%xmm0
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1827
    return (double)(t1->tv_sec - t0->tv_sec)
  401bca:	66 0f ef c9          	pxor   %xmm1,%xmm1
  401bce:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  401bd3:	48 2b 44 24 50       	sub    0x50(%rsp),%rax
  401bd8:	f2 48 0f 2a c8       	cvtsi2sd %rax,%xmm1
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:488
        }
        
        double itertime_14 = gib_difftimespecs(&begin_timed_1233,
                                               &end_timed_1233);
        
        printf("itertime: %lf\n", itertime_14);
  401bdd:	bf 77 48 40 00       	mov    $0x404877,%edi
  401be2:	b8 01 00 00 00       	mov    $0x1,%eax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1828
            + ((double)(t1->tv_nsec - t0->tv_nsec) / 1000000000.0);
  401be7:	f2 0f 5e 05 09 2d 00 	divsd  0x2d09(%rip),%xmm0        # 4048f8 <__PRETTY_FUNCTION__.3+0x28>
  401bee:	00 
  401bef:	f2 0f 58 c1          	addsd  %xmm1,%xmm0
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:485
        double itertime_14 = gib_difftimespecs(&begin_timed_1233,
  401bf3:	f2 0f 11 44 24 38    	movsd  %xmm0,0x38(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:488
        printf("itertime: %lf\n", itertime_14);
  401bf9:	e8 c2 f4 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401bfe:	49 8b 7d 00          	mov    0x0(%r13),%rdi
  401c02:	49 8b 55 10          	mov    0x10(%r13),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401c06:	48 8d 74 24 38       	lea    0x38(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401c0b:	48 01 df             	add    %rbx,%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:469
         gib_get_iters_param(); iters_timed_1233++) {
  401c0e:	48 83 c3 01          	add    $0x1,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  401c12:	48 0f af fa          	imul   %rdx,%rdi
  401c16:	49 03 7d 18          	add    0x18(%r13),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401c1a:	e8 21 f5 ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:468
    for (long long iters_timed_1233 = 0; iters_timed_1233 <
  401c1f:	48 3b 1d d2 44 00 00 	cmp    0x44d2(%rip),%rbx        # 4060f8 <gib_global_iters_param>
  401c26:	0f 8d 3a 01 00 00    	jge    401d66 <main+0xb86>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:474
        clock_gettime(CLOCK_MONOTONIC_RAW, &begin_timed_1233);
  401c2c:	4c 89 fe             	mov    %r15,%rsi
  401c2f:	bf 04 00 00 00       	mov    $0x4,%edi
  401c34:	e8 67 f4 ff ff       	call   4010a0 <clock_gettime@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:533
    return (vec->upper - vec->lower);
  401c39:	4c 8b 65 08          	mov    0x8(%rbp),%r12
  401c3d:	b8 00 00 00 00       	mov    $0x0,%eax
  401c42:	4c 2b 65 00          	sub    0x0(%rbp),%r12
  401c46:	49 0f 49 c4          	cmovns %r12,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401c4a:	bf 20 00 00 00       	mov    $0x20,%edi
  401c4f:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  401c54:	e8 07 f5 ff ff       	call   401160 <malloc@plt>
  401c59:	49 89 c6             	mov    %rax,%r14
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:515
    if (vec == NULL) {
  401c5c:	48 85 c0             	test   %rax,%rax
  401c5f:	0f 84 38 03 00 00    	je     401f9d <main+0xdbd>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:519
    void *data = (void *) gib_alloc(num * elt_size);
  401c65:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  401c6a:	48 8d 3c c5 00 00 00 	lea    0x0(,%rax,8),%rdi
  401c71:	00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401c72:	e8 e9 f4 ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:520
    if (data == NULL) {
  401c77:	48 85 c0             	test   %rax,%rax
  401c7a:	0f 84 df 02 00 00    	je     401f5f <main+0xd7f>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:525
    vec->upper = num;
  401c80:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:524
    vec->lower = 0;
  401c85:	49 c7 06 00 00 00 00 	movq   $0x0,(%r14)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:526
    vec->elt_size = elt_size;
  401c8c:	49 c7 46 10 08 00 00 	movq   $0x8,0x10(%r14)
  401c93:	00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:525
    vec->upper = num;
  401c94:	49 89 76 08          	mov    %rsi,0x8(%r14)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:527
    vec->data = data;
  401c98:	49 89 46 18          	mov    %rax,0x18(%r14)
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:175
    if (fltIf_1026_1101) {
  401c9c:	4d 85 e4             	test   %r12,%r12
  401c9f:	0f 8e 9b fe ff ff    	jle    401b40 <main+0x960>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401ca5:	48 8b 55 10          	mov    0x10(%rbp),%rdx
  401ca9:	48 0f af 55 00       	imul   0x0(%rbp),%rdx
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
        GibInt fltPrm_1027_1104 = *tmp_1;
  401cae:	48 8b 4d 18          	mov    0x18(%rbp),%rcx
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401cb2:	48 8b 14 11          	mov    (%rcx,%rdx,1),%rdx
  401cb6:	48 89 10             	mov    %rdx,(%rax)
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:175
    if (fltIf_1026_1101) {
  401cb9:	49 83 fc 01          	cmp    $0x1,%r12
  401cbd:	0f 84 e5 fe ff ff    	je     401ba8 <main+0x9c8>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401cc3:	48 8b 55 00          	mov    0x0(%rbp),%rdx
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
        GibInt fltPrm_1027_1104 = *tmp_1;
  401cc7:	48 8b 4d 18          	mov    0x18(%rbp),%rcx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  401ccb:	48 83 c2 01          	add    $0x1,%rdx
  401ccf:	48 0f af 55 10       	imul   0x10(%rbp),%rdx
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401cd4:	48 8b 14 11          	mov    (%rcx,%rdx,1),%rdx
  401cd8:	48 89 50 08          	mov    %rdx,0x8(%rax)
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:175
    if (fltIf_1026_1101) {
  401cdc:	49 83 fc 02          	cmp    $0x2,%r12
  401ce0:	0f 84 23 02 00 00    	je     401f09 <main+0xd29>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401ce6:	48 8b 55 00          	mov    0x0(%rbp),%rdx
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
        GibInt fltPrm_1027_1104 = *tmp_1;
  401cea:	48 8b 4d 18          	mov    0x18(%rbp),%rcx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  401cee:	48 83 c2 02          	add    $0x2,%rdx
  401cf2:	48 0f af 55 10       	imul   0x10(%rbp),%rdx
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401cf7:	48 8b 14 11          	mov    (%rcx,%rdx,1),%rdx
  401cfb:	48 89 50 10          	mov    %rdx,0x10(%rax)
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:175
    if (fltIf_1026_1101) {
  401cff:	49 83 fc 03          	cmp    $0x3,%r12
  401d03:	74 35                	je     401d3a <main+0xb5a>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401d05:	48 8b 55 00          	mov    0x0(%rbp),%rdx
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
        GibInt fltPrm_1027_1104 = *tmp_1;
  401d09:	48 8b 4d 18          	mov    0x18(%rbp),%rcx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  401d0d:	48 83 c2 03          	add    $0x3,%rdx
  401d11:	48 0f af 55 10       	imul   0x10(%rbp),%rdx
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401d16:	48 8b 14 11          	mov    (%rcx,%rdx,1),%rdx
  401d1a:	48 89 50 18          	mov    %rdx,0x18(%rax)
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:175
    if (fltIf_1026_1101) {
  401d1e:	49 83 fc 04          	cmp    $0x4,%r12
  401d22:	74 16                	je     401d3a <main+0xb5a>
  401d24:	48 89 f2             	mov    %rsi,%rdx
  401d27:	4c 89 f7             	mov    %r14,%rdi
  401d2a:	48 89 e9             	mov    %rbp,%rcx
  401d2d:	be 04 00 00 00       	mov    $0x4,%esi
  401d32:	e8 49 07 00 00       	call   402480 <generate_loop_544_804.part.0>
  401d37:	49 89 c6             	mov    %rax,%r14
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:331
GibVector *go_538_810(GibInt i_82_951_1174, GibInt n_83_952_1175,
  401d3a:	4c 89 f6             	mov    %r14,%rsi
  401d3d:	bf 01 00 00 00       	mov    $0x1,%edi
  401d42:	e8 c9 09 00 00       	call   402710 <shift_543_811.part.0>
  401d47:	bf 02 00 00 00       	mov    $0x2,%edi
  401d4c:	48 89 c6             	mov    %rax,%rsi
  401d4f:	e8 bc 09 00 00       	call   402710 <shift_543_811.part.0>
  401d54:	48 89 c6             	mov    %rax,%rsi
go_538_810():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
    if (fltIf_1046_1177) {
  401d57:	49 83 fc 03          	cmp    $0x3,%r12
  401d5b:	0f 84 47 fe ff ff    	je     401ba8 <main+0x9c8>
  401d61:	e9 f7 fd ff ff       	jmp    401b5d <main+0x97d>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401d66:	49 8b 55 10          	mov    0x10(%r13),%rdx
  401d6a:	49 8b 7d 00          	mov    0x0(%r13),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:599
    qsort(start, gib_vector_length(vec), vec->elt_size, compar);
  401d6e:	b9 90 21 40 00       	mov    $0x402190,%ecx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:533
    return (vec->upper - vec->lower);
  401d73:	49 8b 75 08          	mov    0x8(%r13),%rsi
  401d77:	48 29 fe             	sub    %rdi,%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401d7a:	48 0f af fa          	imul   %rdx,%rdi
  401d7e:	49 03 7d 18          	add    0x18(%r13),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:599
    qsort(start, gib_vector_length(vec), vec->elt_size, compar);
  401d82:	e8 09 f3 ff ff       	call   401090 <qsort@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:493
        gib_vector_inplace_update(times_17, iters_timed_1233, &itertime_14);
    }
    gib_vector_inplace_sort(times_17, gib_compare_doubles);
    
    double *tmp_18 = (double *) gib_vector_nth(times_17, gib_get_iters_param() /
  401d87:	48 8b 05 6a 43 00 00 	mov    0x436a(%rip),%rax        # 4060f8 <gib_global_iters_param>
  401d8e:	41 b8 02 00 00 00    	mov    $0x2,%r8d
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401d94:	49 8b 7d 00          	mov    0x0(%r13),%rdi
  401d98:	49 8b 75 10          	mov    0x10(%r13),%rsi
  401d9c:	49 8b 4d 18          	mov    0x18(%r13),%rcx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:693
    double acc = 0;
  401da0:	66 0f ef c0          	pxor   %xmm0,%xmm0
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:493
  401da4:	48 99                	cqto
  401da6:	49 f7 f8             	idiv   %r8
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:533
    return (vec->upper - vec->lower);
  401da9:	49 8b 55 08          	mov    0x8(%r13),%rdx
  401dad:	48 29 fa             	sub    %rdi,%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401db0:	48 01 f8             	add    %rdi,%rax
  401db3:	48 0f af c6          	imul   %rsi,%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:495
                                               2);
    double selftimed_16 = *tmp_18;
  401db7:	f2 0f 10 0c 01       	movsd  (%rcx,%rax,1),%xmm1
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:694
    for(int i = 0; i < gib_vector_length(times); i++) {
  401dbc:	48 85 d2             	test   %rdx,%rdx
  401dbf:	7e 23                	jle    401de4 <main+0xc04>
  401dc1:	48 0f af fe          	imul   %rsi,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:693
    double acc = 0;
  401dc5:	66 0f ef c0          	pxor   %xmm0,%xmm0
  401dc9:	48 8d 04 39          	lea    (%rcx,%rdi,1),%rax
  401dcd:	0f 1f 00             	nopl   (%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:694
    for(int i = 0; i < gib_vector_length(times); i++) {
  401dd0:	83 44 24 08 01       	addl   $0x1,0x8(%rsp)
  401dd5:	8b 5c 24 08          	mov    0x8(%rsp),%ebx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:696
        acc += *d;
  401dd9:	f2 0f 58 00          	addsd  (%rax),%xmm0
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:694
    for(int i = 0; i < gib_vector_length(times); i++) {
  401ddd:	48 01 f0             	add    %rsi,%rax
  401de0:	39 d3                	cmp    %edx,%ebx
  401de2:	75 ec                	jne    401dd0 <main+0xbf0>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:498
    double batchtime_15 = gib_sum_timing_array(times_17);
    
    gib_print_timing_array(times_17);
  401de4:	4c 89 ef             	mov    %r13,%rdi
  401de7:	f2 0f 11 4c 24 08    	movsd  %xmm1,0x8(%rsp)
  401ded:	f2 0f 11 44 24 10    	movsd  %xmm0,0x10(%rsp)
  401df3:	e8 98 0b 00 00       	call   402990 <gib_print_timing_array>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:242
void gib_free(void *ptr) { free(ptr); }
  401df8:	49 8b 7d 18          	mov    0x18(%r13),%rdi
  401dfc:	e8 2f f2 ff ff       	call   401030 <free@plt>
  401e01:	4c 89 ef             	mov    %r13,%rdi
  401e04:	e8 27 f2 ff ff       	call   401030 <free@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:500
    gib_vector_free(times_17);
    printf("ITERS: %ld\n", gib_get_iters_param());
  401e09:	48 8b 35 e8 42 00 00 	mov    0x42e8(%rip),%rsi        # 4060f8 <gib_global_iters_param>
  401e10:	bf 86 48 40 00       	mov    $0x404886,%edi
  401e15:	31 c0                	xor    %eax,%eax
  401e17:	e8 a4 f2 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:501
    printf("SIZE: %ld\n", gib_get_size_param());
  401e1c:	48 8b 35 dd 42 00 00 	mov    0x42dd(%rip),%rsi        # 406100 <gib_global_size_param>
  401e23:	bf 92 48 40 00       	mov    $0x404892,%edi
  401e28:	31 c0                	xor    %eax,%eax
  401e2a:	e8 91 f2 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:502
    printf("BATCHTIME: %e\n", batchtime_15);
  401e2f:	f2 0f 10 44 24 10    	movsd  0x10(%rsp),%xmm0
  401e35:	bf 9d 48 40 00       	mov    $0x40489d,%edi
  401e3a:	b8 01 00 00 00       	mov    $0x1,%eax
  401e3f:	e8 7c f2 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:503
    printf("SELFTIMED: %e\n", selftimed_16);
  401e44:	f2 0f 10 4c 24 08    	movsd  0x8(%rsp),%xmm1
  401e4a:	bf ac 48 40 00       	mov    $0x4048ac,%edi
  401e4f:	b8 01 00 00 00       	mov    $0x1,%eax
  401e54:	66 0f 28 c1          	movapd %xmm1,%xmm0
  401e58:	e8 63 f2 ff ff       	call   4010c0 <printf@plt>
printVec_535_799():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:116
    unsigned char wildcard__178_109_900_1070 = gib_print_symbol(1237);
  401e5d:	bf d5 04 00 00       	mov    $0x4d5,%edi
  401e62:	e8 89 11 00 00       	call   402ff0 <gib_print_symbol.isra.0>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:533
    return (vec->upper - vec->lower);
  401e67:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  401e6c:	48 8b 72 08          	mov    0x8(%rdx),%rsi
printVec_loop_545_802():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:130
    if (fltIf_1021_1082) {
  401e70:	48 2b 32             	sub    (%rdx),%rsi
  401e73:	74 07                	je     401e7c <main+0xc9c>
  401e75:	31 ff                	xor    %edi,%edi
  401e77:	e8 04 13 00 00       	call   403180 <printVec_loop_545_802.part.0.isra.0>
printVec_535_799():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:120
    unsigned char wildcard__173_111_902_1073 = gib_print_symbol(1236);
  401e7c:	bf d4 04 00 00       	mov    $0x4d4,%edi
  401e81:	e8 6a 11 00 00       	call   402ff0 <gib_print_symbol.isra.0>
main():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:507
    
    unsigned char wildcard__33_48_863_1063 =  printVec_535_799(timed_1232);
    
    printf("'#(");
  401e86:	bf bb 48 40 00       	mov    $0x4048bb,%edi
  401e8b:	31 c0                	xor    %eax,%eax
  401e8d:	e8 2e f2 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:508
    printf("<vector>");
  401e92:	bf bf 48 40 00       	mov    $0x4048bf,%edi
  401e97:	31 c0                	xor    %eax,%eax
  401e99:	e8 22 f2 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:509
    printf(" ");
  401e9e:	bf 20 00 00 00       	mov    $0x20,%edi
  401ea3:	e8 98 f1 ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:510
    printf("<vector>");
  401ea8:	bf bf 48 40 00       	mov    $0x4048bf,%edi
  401ead:	31 c0                	xor    %eax,%eax
  401eaf:	e8 0c f2 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:511
    printf(")");
  401eb4:	bf 29 00 00 00       	mov    $0x29,%edi
  401eb9:	e8 82 f1 ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:512
    printf("\n");
  401ebe:	bf 0a 00 00 00       	mov    $0xa,%edi
  401ec3:	e8 78 f1 ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:242
void gib_free(void *ptr) { free(ptr); }
  401ec8:	48 8b 3d b9 42 00 00 	mov    0x42b9(%rip),%rdi        # 406188 <gib_global_bench_prog_param>
  401ecf:	e8 5c f1 ff ff       	call   401030 <free@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:2042
    GibShadowstack *rstack = DEFAULT_READ_SHADOWSTACK;
    GibShadowstack *wstack = DEFAULT_WRITE_SHADOWSTACK;
    GibOldgen *oldgen = DEFAULT_GENERATION;

    // Free all objects initialized by the Rust RTS.
    gib_gc_cleanup(rstack, wstack, nursery, oldgen);
  401ed4:	48 8b 0d 8d 42 00 00 	mov    0x428d(%rip),%rcx        # 406168 <gib_global_oldgen>
  401edb:	48 8b 15 9e 42 00 00 	mov    0x429e(%rip),%rdx        # 406180 <gib_global_nurseries>
  401ee2:	48 8b 35 87 42 00 00 	mov    0x4287(%rip),%rsi        # 406170 <gib_global_write_shadowstacks>
  401ee9:	48 8b 3d 88 42 00 00 	mov    0x4288(%rip),%rdi        # 406178 <gib_global_read_shadowstacks>
  401ef0:	e8 db f1 ff ff       	call   4010d0 <gib_gc_cleanup@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:517
    
    int exit_21 = gib_exit();
    
    return exit_21;
  401ef5:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  401efc:	31 c0                	xor    %eax,%eax
  401efe:	5b                   	pop    %rbx
  401eff:	5d                   	pop    %rbp
  401f00:	41 5c                	pop    %r12
  401f02:	41 5d                	pop    %r13
  401f04:	41 5e                	pop    %r14
  401f06:	41 5f                	pop    %r15
  401f08:	c3                   	ret
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:331
GibVector *go_538_810(GibInt i_82_951_1174, GibInt n_83_952_1175,
  401f09:	4c 89 f6             	mov    %r14,%rsi
  401f0c:	bf 01 00 00 00       	mov    $0x1,%edi
  401f11:	e8 fa 07 00 00       	call   402710 <shift_543_811.part.0>
go_538_810():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:331
  401f16:	e9 8d fc ff ff       	jmp    401ba8 <main+0x9c8>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401f1b:	bf 01 00 00 00       	mov    $0x1,%edi
  401f20:	e8 3b f2 ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1997
        gib_global_bench_prog_param = (char*) gib_alloc(1*sizeof(char));
  401f25:	48 89 05 5c 42 00 00 	mov    %rax,0x425c(%rip)        # 406188 <gib_global_bench_prog_param>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1998
        *gib_global_bench_prog_param = '\n';
  401f2c:	c6 00 0a             	movb   $0xa,(%rax)
  401f2f:	e9 a2 f6 ff ff       	jmp    4015d6 <main+0x3f6>
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1998
  401f34:	4c 89 f6             	mov    %r14,%rsi
  401f37:	48 89 ef             	mov    %rbp,%rdi
  401f3a:	e8 81 06 00 00       	call   4025c0 <isort_540_807.part.0>
  401f3f:	49 89 c6             	mov    %rax,%r14
  401f42:	e9 9b fa ff ff       	jmp    4019e2 <main+0x802>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:693
    double acc = 0;
  401f47:	66 0f ef db          	pxor   %xmm3,%xmm3
  401f4b:	f2 0f 11 5c 24 20    	movsd  %xmm3,0x20(%rsp)
  401f51:	66 0f 28 c3          	movapd %xmm3,%xmm0
  401f55:	e9 3c fb ff ff       	jmp    401a96 <main+0x8b6>
  401f5a:	e8 01 0a 00 00       	call   402960 <gib_shadowstack_initialize.part.0.constprop.0>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:521
        fprintf(stderr, "alloc_vector: gib_alloc failed: %zu", sizeof(num * elt_size));
  401f5f:	48 8b 3d da 41 00 00 	mov    0x41da(%rip),%rdi        # 406140 <stderr@GLIBC_2.2.5>
  401f66:	ba 08 00 00 00       	mov    $0x8,%edx
  401f6b:	be 08 40 40 00       	mov    $0x404008,%esi
  401f70:	e8 bb f1 ff ff       	call   401130 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:522
        exit(1);
  401f75:	bf 01 00 00 00       	mov    $0x1,%edi
  401f7a:	e8 31 f2 ff ff       	call   4011b0 <exit@plt>
  401f7f:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  401f84:	be c1 47 40 00       	mov    $0x4047c1,%esi
  401f89:	e8 62 0b 00 00       	call   402af0 <check_args.part.0>
  401f8e:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  401f93:	be d6 47 40 00       	mov    $0x4047d6,%esi
  401f98:	e8 53 0b 00 00       	call   402af0 <check_args.part.0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:516
        fprintf(stderr, "alloc_vector: gib_alloc failed: %zu", sizeof(GibVector));
  401f9d:	ba 20 00 00 00       	mov    $0x20,%edx
  401fa2:	be 08 40 40 00       	mov    $0x404008,%esi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1459
        fprintf(stderr, "gib_oldgen_initialize: gib_alloc failed: %zu",
  401fa7:	48 8b 3d 92 41 00 00 	mov    0x4192(%rip),%rdi        # 406140 <stderr@GLIBC_2.2.5>
  401fae:	31 c0                	xor    %eax,%eax
  401fb0:	e8 7b f1 ff ff       	call   401130 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1461
        exit(1);
  401fb5:	bf 01 00 00 00       	mov    $0x1,%edi
  401fba:	e8 f1 f1 ff ff       	call   4011b0 <exit@plt>
info_table_initialize():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:90
        fprintf(stderr, "Couldn't initialize info table, errorno=%d", error);
  401fbf:	89 c2                	mov    %eax,%edx
  401fc1:	be d8 46 40 00       	mov    $0x4046d8,%esi
  401fc6:	48 8b 3d 73 41 00 00 	mov    0x4173(%rip),%rdi        # 406140 <stderr@GLIBC_2.2.5>
  401fcd:	31 c0                	xor    %eax,%eax
  401fcf:	e8 5c f1 ff ff       	call   401130 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:91
        exit(1);
  401fd4:	bf 01 00 00 00       	mov    $0x1,%edi
  401fd9:	e8 d2 f1 ff ff       	call   4011b0 <exit@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1459
        fprintf(stderr, "gib_oldgen_initialize: gib_alloc failed: %zu",
  401fde:	ba 18 00 00 00       	mov    $0x18,%edx
  401fe3:	be a8 46 40 00       	mov    $0x4046a8,%esi
  401fe8:	eb bd                	jmp    401fa7 <main+0xdc7>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1423
        fprintf(stderr, "gib_nursery_initialize: gib_alloc failed: %zu",
  401fea:	ba 00 00 40 00       	mov    $0x400000,%edx
  401fef:	be 40 46 40 00       	mov    $0x404640,%esi
  401ff4:	eb b1                	jmp    401fa7 <main+0xdc7>
  401ff6:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  401ffb:	be 04 48 40 00       	mov    $0x404804,%esi
  402000:	e8 eb 0a 00 00       	call   402af0 <check_args.part.0>
  402005:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  40200a:	be 30 48 40 00       	mov    $0x404830,%esi
  40200f:	e8 dc 0a 00 00       	call   402af0 <check_args.part.0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1987
            fprintf(stderr, "Extra arguments left over: ");
  402014:	48 8b 0d 25 41 00 00 	mov    0x4125(%rip),%rcx        # 406140 <stderr@GLIBC_2.2.5>
  40201b:	ba 1b 00 00 00       	mov    $0x1b,%edx
  402020:	be 01 00 00 00       	mov    $0x1,%esi
  402025:	4d 63 ff             	movslq %r15d,%r15
  402028:	bf 3d 48 40 00       	mov    $0x40483d,%edi
  40202d:	e8 8e f1 ff ff       	call   4011c0 <fwrite@plt>
  402032:	eb 20                	jmp    402054 <main+0xe74>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1988
            for(; i < argc; i++) fprintf(stderr, "%s ", argv[i]);
  402034:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402039:	48 8b 3d 00 41 00 00 	mov    0x4100(%rip),%rdi        # 406140 <stderr@GLIBC_2.2.5>
  402040:	be 59 48 40 00       	mov    $0x404859,%esi
  402045:	4a 8b 14 f8          	mov    (%rax,%r15,8),%rdx
  402049:	31 c0                	xor    %eax,%eax
  40204b:	49 83 c7 01          	add    $0x1,%r15
  40204f:	e8 dc f0 ff ff       	call   401130 <fprintf@plt>
  402054:	44 39 fb             	cmp    %r15d,%ebx
  402057:	7f db                	jg     402034 <main+0xe54>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1989
            gib_show_usage(argv);
  402059:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  40205e:	e8 cd 09 00 00       	call   402a30 <gib_show_usage>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1990
            exit(1);
  402063:	bf 01 00 00 00       	mov    $0x1,%edi
  402068:	e8 43 f1 ff ff       	call   4011b0 <exit@plt>
  40206d:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  402072:	be 19 48 40 00       	mov    $0x404819,%esi
  402077:	e8 74 0a 00 00       	call   402af0 <check_args.part.0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1910
        fprintf(stderr, " [gibbon rts] failed to getrlimit, code %d\n", code);
  40207c:	89 c2                	mov    %eax,%edx
  40207e:	be 88 45 40 00       	mov    $0x404588,%esi
  402083:	e9 3e ff ff ff       	jmp    401fc6 <main+0xde6>
  402088:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  40208d:	be 26 48 40 00       	mov    $0x404826,%esi
  402092:	e8 59 0a 00 00       	call   402af0 <check_args.part.0>
  402097:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40209e:	00 00 

00000000004020a0 <_start>:
_start():
  4020a0:	f3 0f 1e fa          	endbr64
  4020a4:	31 ed                	xor    %ebp,%ebp
  4020a6:	49 89 d1             	mov    %rdx,%r9
  4020a9:	5e                   	pop    %rsi
  4020aa:	48 89 e2             	mov    %rsp,%rdx
  4020ad:	48 83 e4 f0          	and    $0xfffffffffffffff0,%rsp
  4020b1:	50                   	push   %rax
  4020b2:	54                   	push   %rsp
  4020b3:	45 31 c0             	xor    %r8d,%r8d
  4020b6:	31 c9                	xor    %ecx,%ecx
  4020b8:	48 c7 c7 e0 11 40 00 	mov    $0x4011e0,%rdi
  4020bf:	ff 15 13 3f 00 00    	call   *0x3f13(%rip)        # 405fd8 <__libc_start_main@GLIBC_2.34>
  4020c5:	f4                   	hlt
  4020c6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4020cd:	00 00 00 

00000000004020d0 <_dl_relocate_static_pie>:
_dl_relocate_static_pie():
  4020d0:	f3 0f 1e fa          	endbr64
  4020d4:	c3                   	ret
  4020d5:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4020dc:	00 00 00 
  4020df:	90                   	nop

00000000004020e0 <deregister_tm_clones>:
deregister_tm_clones():
  4020e0:	b8 08 61 40 00       	mov    $0x406108,%eax
  4020e5:	48 3d 08 61 40 00    	cmp    $0x406108,%rax
  4020eb:	74 13                	je     402100 <deregister_tm_clones+0x20>
  4020ed:	b8 00 00 00 00       	mov    $0x0,%eax
  4020f2:	48 85 c0             	test   %rax,%rax
  4020f5:	74 09                	je     402100 <deregister_tm_clones+0x20>
  4020f7:	bf 08 61 40 00       	mov    $0x406108,%edi
  4020fc:	ff e0                	jmp    *%rax
  4020fe:	66 90                	xchg   %ax,%ax
  402100:	c3                   	ret
  402101:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  402108:	00 00 00 00 
  40210c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402110 <register_tm_clones>:
register_tm_clones():
  402110:	be 08 61 40 00       	mov    $0x406108,%esi
  402115:	48 81 ee 08 61 40 00 	sub    $0x406108,%rsi
  40211c:	48 89 f0             	mov    %rsi,%rax
  40211f:	48 c1 ee 3f          	shr    $0x3f,%rsi
  402123:	48 c1 f8 03          	sar    $0x3,%rax
  402127:	48 01 c6             	add    %rax,%rsi
  40212a:	48 d1 fe             	sar    $1,%rsi
  40212d:	74 11                	je     402140 <register_tm_clones+0x30>
  40212f:	b8 00 00 00 00       	mov    $0x0,%eax
  402134:	48 85 c0             	test   %rax,%rax
  402137:	74 07                	je     402140 <register_tm_clones+0x30>
  402139:	bf 08 61 40 00       	mov    $0x406108,%edi
  40213e:	ff e0                	jmp    *%rax
  402140:	c3                   	ret
  402141:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  402148:	00 00 00 00 
  40214c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402150 <__do_global_dtors_aux>:
__do_global_dtors_aux():
  402150:	80 3d f1 3f 00 00 00 	cmpb   $0x0,0x3ff1(%rip)        # 406148 <completed.0>
  402157:	75 17                	jne    402170 <__do_global_dtors_aux+0x20>
  402159:	55                   	push   %rbp
  40215a:	48 89 e5             	mov    %rsp,%rbp
  40215d:	e8 7e ff ff ff       	call   4020e0 <deregister_tm_clones>
  402162:	c6 05 df 3f 00 00 01 	movb   $0x1,0x3fdf(%rip)        # 406148 <completed.0>
  402169:	5d                   	pop    %rbp
  40216a:	c3                   	ret
  40216b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
  402170:	c3                   	ret
  402171:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  402178:	00 00 00 00 
  40217c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402180 <frame_dummy>:
frame_dummy():
  402180:	eb 8e                	jmp    402110 <register_tm_clones>
  402182:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  402189:	00 00 00 
  40218c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402190 <gib_compare_doubles>:
gib_compare_doubles():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1835
    return (*da > *db) - (*da < *db);
  402190:	f2 0f 10 07          	movsd  (%rdi),%xmm0
  402194:	f2 0f 10 0e          	movsd  (%rsi),%xmm1
  402198:	31 c0                	xor    %eax,%eax
  40219a:	66 0f 2f c1          	comisd %xmm1,%xmm0
  40219e:	0f 97 c0             	seta   %al
  4021a1:	31 d2                	xor    %edx,%edx
  4021a3:	66 0f 2f c8          	comisd %xmm0,%xmm1
  4021a7:	0f 97 c2             	seta   %dl
  4021aa:	29 d0                	sub    %edx,%eax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1836
}
  4021ac:	c3                   	ret
  4021ad:	0f 1f 00             	nopl   (%rax)

00000000004021b0 <generate_loop_544_808>:
generate_loop_544_808():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:217
{
  4021b0:	41 56                	push   %r14
  4021b2:	41 55                	push   %r13
  4021b4:	41 54                	push   %r12
  4021b6:	49 89 fc             	mov    %rdi,%r12
  4021b9:	53                   	push   %rbx
  4021ba:	48 83 ec 38          	sub    $0x38,%rsp
  4021be:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:220
    if (fltIf_1031_1126) {
  4021c3:	48 39 d6             	cmp    %rdx,%rsi
  4021c6:	75 18                	jne    4021e0 <generate_loop_544_808+0x30>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:232
}
  4021c8:	48 83 c4 38          	add    $0x38,%rsp
  4021cc:	4c 89 e0             	mov    %r12,%rax
  4021cf:	5b                   	pop    %rbx
  4021d0:	41 5c                	pop    %r12
  4021d2:	41 5d                	pop    %r13
  4021d4:	41 5e                	pop    %r14
  4021d6:	c3                   	ret
  4021d7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  4021de:	00 00 
  4021e0:	49 89 d5             	mov    %rdx,%r13
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4021e3:	48 8b 57 10          	mov    0x10(%rdi),%rdx
  4021e7:	48 8b 3f             	mov    (%rdi),%rdi
  4021ea:	48 89 f3             	mov    %rsi,%rbx
  4021ed:	48 01 f7             	add    %rsi,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4021f0:	48 8d 74 24 08       	lea    0x8(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4021f5:	48 0f af fa          	imul   %rdx,%rdi
  4021f9:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4021fe:	e8 3d ef ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:228
                   generate_loop_544_808(vec1_259_938_1130, fltAppE_1033_1131, end_256_936_1124, hd_57_937_1125);
  402203:	4c 8b 74 24 08       	mov    0x8(%rsp),%r14
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:226
        GibInt fltAppE_1033_1131 = idx_255_935_1123 + 1;
  402208:	48 8d 7b 01          	lea    0x1(%rbx),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:227
        GibVector *tailapp_1221 =
  40220c:	4c 89 74 24 18       	mov    %r14,0x18(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:220
    if (fltIf_1031_1126) {
  402211:	49 39 fd             	cmp    %rdi,%r13
  402214:	74 b2                	je     4021c8 <generate_loop_544_808+0x18>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402216:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
  40221b:	49 03 3c 24          	add    (%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  40221f:	48 8d 74 24 18       	lea    0x18(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402224:	48 0f af fa          	imul   %rdx,%rdi
  402228:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  40222d:	e8 0e ef ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:226
        GibInt fltAppE_1033_1131 = idx_255_935_1123 + 1;
  402232:	48 8d 7b 02          	lea    0x2(%rbx),%rdi
  402236:	4c 89 74 24 20       	mov    %r14,0x20(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:220
    if (fltIf_1031_1126) {
  40223b:	49 39 fd             	cmp    %rdi,%r13
  40223e:	74 88                	je     4021c8 <generate_loop_544_808+0x18>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402240:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
  402245:	49 03 3c 24          	add    (%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402249:	48 8d 74 24 20       	lea    0x20(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  40224e:	48 0f af fa          	imul   %rdx,%rdi
  402252:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402257:	e8 e4 ee ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:226
        GibInt fltAppE_1033_1131 = idx_255_935_1123 + 1;
  40225c:	48 8d 7b 03          	lea    0x3(%rbx),%rdi
  402260:	4c 89 74 24 28       	mov    %r14,0x28(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:220
    if (fltIf_1031_1126) {
  402265:	49 39 fd             	cmp    %rdi,%r13
  402268:	0f 84 5a ff ff ff    	je     4021c8 <generate_loop_544_808+0x18>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  40226e:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
  402273:	49 03 3c 24          	add    (%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402277:	48 8d 74 24 28       	lea    0x28(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  40227c:	48 0f af fa          	imul   %rdx,%rdi
  402280:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402285:	e8 b6 ee ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:228
                   generate_loop_544_808(vec1_259_938_1130, fltAppE_1033_1131, end_256_936_1124, hd_57_937_1125);
  40228a:	4c 89 e7             	mov    %r12,%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:226
        GibInt fltAppE_1033_1131 = idx_255_935_1123 + 1;
  40228d:	48 8d 73 04          	lea    0x4(%rbx),%rsi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:228
                   generate_loop_544_808(vec1_259_938_1130, fltAppE_1033_1131, end_256_936_1124, hd_57_937_1125);
  402291:	4c 89 f1             	mov    %r14,%rcx
  402294:	4c 89 ea             	mov    %r13,%rdx
  402297:	e8 14 ff ff ff       	call   4021b0 <generate_loop_544_808>
  40229c:	49 89 c4             	mov    %rax,%r12
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:230
        return tailapp_1221;
  40229f:	e9 24 ff ff ff       	jmp    4021c8 <generate_loop_544_808+0x18>
  4022a4:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  4022ab:	00 00 00 00 
  4022af:	90                   	nop

00000000004022b0 <insert_541_809>:
insert_541_809():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:264
{
  4022b0:	41 55                	push   %r13
  4022b2:	41 54                	push   %r12
  4022b4:	55                   	push   %rbp
  4022b5:	48 89 fd             	mov    %rdi,%rbp
  4022b8:	48 83 ec 20          	sub    $0x20,%rsp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4022bc:	48 8b 7f 18          	mov    0x18(%rdi),%rdi
  4022c0:	4c 8b 45 10          	mov    0x10(%rbp),%r8
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:264
  4022c4:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4022c9:	48 8b 45 00          	mov    0x0(%rbp),%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:267
    if (fltIf_1038_1147) {
  4022cd:	48 85 d2             	test   %rdx,%rdx
  4022d0:	74 6e                	je     402340 <insert_541_809+0x90>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:273
        GibInt i_98_883_996_1152 = n_70_946_1146 - 1;
  4022d2:	4c 8d 62 ff          	lea    -0x1(%rdx),%r12
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4022d6:	48 01 c2             	add    %rax,%rdx
  4022d9:	49 89 f5             	mov    %rsi,%r13
  4022dc:	49 0f af d0          	imul   %r8,%rdx
  4022e0:	49 8d 0c 04          	lea    (%r12,%rax,1),%rcx
  4022e4:	49 0f af c8          	imul   %r8,%rcx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:278
        GibInt y_72_947_1153 = *tmp_5;
  4022e8:	48 8b 0c 0f          	mov    (%rdi,%rcx,1),%rcx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4022ec:	48 01 d7             	add    %rdx,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4022ef:	4c 89 c2             	mov    %r8,%rdx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:278
  4022f2:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:282
        if (fltIf_1041_1156) {
  4022f7:	48 39 f1             	cmp    %rsi,%rcx
  4022fa:	7f 1c                	jg     402318 <insert_541_809+0x68>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
  4022fc:	48 8d 74 24 08       	lea    0x8(%rsp),%rsi
  402301:	e8 3a ee ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:315
}
  402306:	48 83 c4 20          	add    $0x20,%rsp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
  40230a:	48 89 e8             	mov    %rbp,%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:315
  40230d:	5d                   	pop    %rbp
  40230e:	41 5c                	pop    %r12
  402310:	41 5d                	pop    %r13
  402312:	c3                   	ret
  402313:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
  402318:	48 8d 74 24 18       	lea    0x18(%rsp),%rsi
  40231d:	e8 1e ee ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:304
                       insert_541_809(xs__73_948_1163, x_69_945_1145, fltAppE_1043_1164);
  402322:	4c 89 e2             	mov    %r12,%rdx
  402325:	4c 89 ee             	mov    %r13,%rsi
  402328:	48 89 ef             	mov    %rbp,%rdi
  40232b:	e8 80 ff ff ff       	call   4022b0 <insert_541_809>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:315
}
  402330:	48 83 c4 20          	add    $0x20,%rsp
  402334:	5d                   	pop    %rbp
  402335:	41 5c                	pop    %r12
  402337:	41 5d                	pop    %r13
  402339:	c3                   	ret
  40233a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402340:	49 0f af c0          	imul   %r8,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402344:	48 8d 74 24 08       	lea    0x8(%rsp),%rsi
  402349:	4c 89 c2             	mov    %r8,%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  40234c:	48 01 c7             	add    %rax,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  40234f:	e8 ec ed ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:315
  402354:	48 83 c4 20          	add    $0x20,%rsp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
  402358:	48 89 e8             	mov    %rbp,%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:315
  40235b:	5d                   	pop    %rbp
  40235c:	41 5c                	pop    %r12
  40235e:	41 5d                	pop    %r13
  402360:	c3                   	ret
  402361:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  402368:	00 00 00 00 
  40236c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402370 <generate_loop_544_803.part.0>:
generate_loop_544_803():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:148
GibVector *generate_loop_544_803(GibVector *vec_254_913_1087,
  402370:	41 56                	push   %r14
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
        GibInt fltPrm_1024_1094 = n_42_916_1090 - idx_255_914_1088;
  402372:	48 89 c8             	mov    %rcx,%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:148
GibVector *generate_loop_544_803(GibVector *vec_254_913_1087,
  402375:	49 89 ce             	mov    %rcx,%r14
  402378:	41 55                	push   %r13
  40237a:	49 89 d5             	mov    %rdx,%r13
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
        GibInt fltPrm_1024_1094 = n_42_916_1090 - idx_255_914_1088;
  40237d:	48 29 f0             	sub    %rsi,%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:148
GibVector *generate_loop_544_803(GibVector *vec_254_913_1087,
  402380:	41 54                	push   %r12
  402382:	49 89 fc             	mov    %rdi,%r12
  402385:	53                   	push   %rbx
  402386:	48 89 f3             	mov    %rsi,%rbx
  402389:	48 83 ec 28          	sub    $0x28,%rsp
generate_loop_544_803.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  40238d:	48 8b 57 10          	mov    0x10(%rdi),%rdx
  402391:	48 8b 3f             	mov    (%rdi),%rdi
generate_loop_544_803():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
        GibInt fltPrm_1024_1094 = n_42_916_1090 - idx_255_914_1088;
  402394:	48 89 04 24          	mov    %rax,(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402398:	48 01 f7             	add    %rsi,%rdi
generate_loop_544_803.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  40239b:	48 89 e6             	mov    %rsp,%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  40239e:	48 0f af fa          	imul   %rdx,%rdi
  4023a2:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4023a7:	e8 94 ed ff ff       	call   401140 <memcpy@plt>
generate_loop_544_803():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:161
        GibInt fltAppE_1025_1096 = idx_255_914_1088 + 1;
  4023ac:	48 8d 7b 01          	lea    0x1(%rbx),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:154
    if (fltIf_1023_1091) {
  4023b0:	4c 39 ef             	cmp    %r13,%rdi
  4023b3:	75 13                	jne    4023c8 <generate_loop_544_803.part.0+0x58>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:167
}
  4023b5:	48 83 c4 28          	add    $0x28,%rsp
  4023b9:	4c 89 e0             	mov    %r12,%rax
  4023bc:	5b                   	pop    %rbx
  4023bd:	41 5c                	pop    %r12
  4023bf:	41 5d                	pop    %r13
  4023c1:	41 5e                	pop    %r14
  4023c3:	c3                   	ret
  4023c4:	0f 1f 40 00          	nopl   0x0(%rax)
generate_loop_544_803.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4023c8:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
generate_loop_544_803():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
        GibInt fltPrm_1024_1094 = n_42_916_1090 - idx_255_914_1088;
  4023cd:	4c 89 f0             	mov    %r14,%rax
generate_loop_544_803.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4023d0:	48 8d 74 24 08       	lea    0x8(%rsp),%rsi
generate_loop_544_803():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
  4023d5:	48 29 f8             	sub    %rdi,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4023d8:	49 03 3c 24          	add    (%r12),%rdi
  4023dc:	48 0f af fa          	imul   %rdx,%rdi
  4023e0:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
  4023e5:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4023ea:	e8 51 ed ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:161
        GibInt fltAppE_1025_1096 = idx_255_914_1088 + 1;
  4023ef:	48 8d 7b 02          	lea    0x2(%rbx),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:154
    if (fltIf_1023_1091) {
  4023f3:	49 39 fd             	cmp    %rdi,%r13
  4023f6:	74 bd                	je     4023b5 <generate_loop_544_803.part.0+0x45>
generate_loop_544_803.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4023f8:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
generate_loop_544_803():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
        GibInt fltPrm_1024_1094 = n_42_916_1090 - idx_255_914_1088;
  4023fd:	4c 89 f0             	mov    %r14,%rax
generate_loop_544_803.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402400:	48 8d 74 24 10       	lea    0x10(%rsp),%rsi
generate_loop_544_803():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
  402405:	48 29 f8             	sub    %rdi,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402408:	49 03 3c 24          	add    (%r12),%rdi
  40240c:	48 0f af fa          	imul   %rdx,%rdi
  402410:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
  402415:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  40241a:	e8 21 ed ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:161
        GibInt fltAppE_1025_1096 = idx_255_914_1088 + 1;
  40241f:	48 8d 7b 03          	lea    0x3(%rbx),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:154
    if (fltIf_1023_1091) {
  402423:	49 39 fd             	cmp    %rdi,%r13
  402426:	74 8d                	je     4023b5 <generate_loop_544_803.part.0+0x45>
generate_loop_544_803.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402428:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
generate_loop_544_803():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
        GibInt fltPrm_1024_1094 = n_42_916_1090 - idx_255_914_1088;
  40242d:	4c 89 f0             	mov    %r14,%rax
generate_loop_544_803.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402430:	48 8d 74 24 18       	lea    0x18(%rsp),%rsi
generate_loop_544_803():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
  402435:	48 29 f8             	sub    %rdi,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402438:	49 03 3c 24          	add    (%r12),%rdi
  40243c:	48 0f af fa          	imul   %rdx,%rdi
  402440:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:157
  402445:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  40244a:	e8 f1 ec ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:161
        GibInt fltAppE_1025_1096 = idx_255_914_1088 + 1;
  40244f:	48 8d 73 04          	lea    0x4(%rbx),%rsi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:154
    if (fltIf_1023_1091) {
  402453:	49 39 f5             	cmp    %rsi,%r13
  402456:	0f 84 59 ff ff ff    	je     4023b5 <generate_loop_544_803.part.0+0x45>
  40245c:	4c 89 e7             	mov    %r12,%rdi
  40245f:	4c 89 f1             	mov    %r14,%rcx
  402462:	4c 89 ea             	mov    %r13,%rdx
  402465:	e8 06 ff ff ff       	call   402370 <generate_loop_544_803.part.0>
  40246a:	49 89 c4             	mov    %rax,%r12
  40246d:	e9 43 ff ff ff       	jmp    4023b5 <generate_loop_544_803.part.0+0x45>
generate_loop_544_803.part.0():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:154
  402472:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  402479:	00 00 00 00 
  40247d:	0f 1f 00             	nopl   (%rax)

0000000000402480 <generate_loop_544_804.part.0>:
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:168
GibVector *generate_loop_544_804(GibVector *vec_254_918_1097,
  402480:	41 56                	push   %r14
  402482:	49 89 d6             	mov    %rdx,%r14
  402485:	41 55                	push   %r13
  402487:	49 89 cd             	mov    %rcx,%r13
  40248a:	41 54                	push   %r12
  40248c:	49 89 fc             	mov    %rdi,%r12
  40248f:	53                   	push   %rbx
  402490:	48 89 f3             	mov    %rsi,%rbx
  402493:	48 83 ec 28          	sub    $0x28,%rsp
generate_loop_544_804.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402497:	48 8b 01             	mov    (%rcx),%rax
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
        GibInt fltPrm_1027_1104 = *tmp_1;
  40249a:	48 8b 51 18          	mov    0x18(%rcx),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  40249e:	48 01 f0             	add    %rsi,%rax
  4024a1:	48 0f af 41 10       	imul   0x10(%rcx),%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
  4024a6:	48 8b 04 02          	mov    (%rdx,%rax,1),%rax
generate_loop_544_804.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4024aa:	48 8b 57 10          	mov    0x10(%rdi),%rdx
  4024ae:	48 8b 3f             	mov    (%rdi),%rdi
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
  4024b1:	48 89 04 24          	mov    %rax,(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4024b5:	48 01 f7             	add    %rsi,%rdi
generate_loop_544_804.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4024b8:	48 89 e6             	mov    %rsp,%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4024bb:	48 0f af fa          	imul   %rdx,%rdi
  4024bf:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4024c4:	e8 77 ec ff ff       	call   401140 <memcpy@plt>
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:186
        GibInt fltAppE_1028_1106 = idx_255_919_1098 + 1;
  4024c9:	48 8d 7b 01          	lea    0x1(%rbx),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:175
    if (fltIf_1026_1101) {
  4024cd:	4c 39 f7             	cmp    %r14,%rdi
  4024d0:	75 16                	jne    4024e8 <generate_loop_544_804.part.0+0x68>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:192
}
  4024d2:	48 83 c4 28          	add    $0x28,%rsp
  4024d6:	4c 89 e0             	mov    %r12,%rax
  4024d9:	5b                   	pop    %rbx
  4024da:	41 5c                	pop    %r12
  4024dc:	41 5d                	pop    %r13
  4024de:	41 5e                	pop    %r14
  4024e0:	c3                   	ret
  4024e1:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)
generate_loop_544_804.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4024e8:	49 8b 45 00          	mov    0x0(%r13),%rax
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
        GibInt fltPrm_1027_1104 = *tmp_1;
  4024ec:	49 8b 55 18          	mov    0x18(%r13),%rdx
generate_loop_544_804.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4024f0:	48 8d 74 24 08       	lea    0x8(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4024f5:	48 01 f8             	add    %rdi,%rax
  4024f8:	49 0f af 45 10       	imul   0x10(%r13),%rax
  4024fd:	49 03 3c 24          	add    (%r12),%rdi
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
  402501:	48 8b 04 02          	mov    (%rdx,%rax,1),%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402505:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
  40250a:	48 0f af fa          	imul   %rdx,%rdi
  40250e:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
  402513:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402518:	e8 23 ec ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:186
        GibInt fltAppE_1028_1106 = idx_255_919_1098 + 1;
  40251d:	48 8d 7b 02          	lea    0x2(%rbx),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:175
    if (fltIf_1026_1101) {
  402521:	49 39 fe             	cmp    %rdi,%r14
  402524:	74 ac                	je     4024d2 <generate_loop_544_804.part.0+0x52>
generate_loop_544_804.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402526:	49 8b 45 00          	mov    0x0(%r13),%rax
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
        GibInt fltPrm_1027_1104 = *tmp_1;
  40252a:	49 8b 55 18          	mov    0x18(%r13),%rdx
generate_loop_544_804.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  40252e:	48 8d 74 24 10       	lea    0x10(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402533:	48 01 f8             	add    %rdi,%rax
  402536:	49 0f af 45 10       	imul   0x10(%r13),%rax
  40253b:	49 03 3c 24          	add    (%r12),%rdi
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
  40253f:	48 8b 04 02          	mov    (%rdx,%rax,1),%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402543:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
  402548:	48 0f af fa          	imul   %rdx,%rdi
  40254c:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
  402551:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402556:	e8 e5 eb ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:186
        GibInt fltAppE_1028_1106 = idx_255_919_1098 + 1;
  40255b:	48 8d 7b 03          	lea    0x3(%rbx),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:175
    if (fltIf_1026_1101) {
  40255f:	49 39 fe             	cmp    %rdi,%r14
  402562:	0f 84 6a ff ff ff    	je     4024d2 <generate_loop_544_804.part.0+0x52>
generate_loop_544_804.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402568:	49 8b 45 00          	mov    0x0(%r13),%rax
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
        GibInt fltPrm_1027_1104 = *tmp_1;
  40256c:	49 8b 55 18          	mov    0x18(%r13),%rdx
generate_loop_544_804.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402570:	48 8d 74 24 18       	lea    0x18(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402575:	48 01 f8             	add    %rdi,%rax
  402578:	49 0f af 45 10       	imul   0x10(%r13),%rax
  40257d:	49 03 3c 24          	add    (%r12),%rdi
generate_loop_544_804():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
  402581:	48 8b 04 02          	mov    (%rdx,%rax,1),%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402585:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
  40258a:	48 0f af fa          	imul   %rdx,%rdi
  40258e:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:182
  402593:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402598:	e8 a3 eb ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:186
        GibInt fltAppE_1028_1106 = idx_255_919_1098 + 1;
  40259d:	48 8d 73 04          	lea    0x4(%rbx),%rsi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:175
    if (fltIf_1026_1101) {
  4025a1:	49 39 f6             	cmp    %rsi,%r14
  4025a4:	0f 84 28 ff ff ff    	je     4024d2 <generate_loop_544_804.part.0+0x52>
  4025aa:	4c 89 e7             	mov    %r12,%rdi
  4025ad:	4c 89 e9             	mov    %r13,%rcx
  4025b0:	4c 89 f2             	mov    %r14,%rdx
  4025b3:	e8 c8 fe ff ff       	call   402480 <generate_loop_544_804.part.0>
  4025b8:	49 89 c4             	mov    %rax,%r12
  4025bb:	e9 12 ff ff ff       	jmp    4024d2 <generate_loop_544_804.part.0+0x52>

00000000004025c0 <isort_540_807.part.0>:
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:233
GibVector *isort_540_807(GibVector *xs_61_939_1132, GibVector *b_62_940_1133,
  4025c0:	41 57                	push   %r15
  4025c2:	41 56                	push   %r14
  4025c4:	41 55                	push   %r13
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:247
            GibInt fltAppE_1036_1139 = n_63_941_1134 - 1;
  4025c6:	4c 8d 6a ff          	lea    -0x1(%rdx),%r13
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:233
GibVector *isort_540_807(GibVector *xs_61_939_1132, GibVector *b_62_940_1133,
  4025ca:	41 54                	push   %r12
  4025cc:	49 89 d4             	mov    %rdx,%r12
  4025cf:	53                   	push   %rbx
  4025d0:	48 89 fb             	mov    %rdi,%rbx
  4025d3:	48 83 ec 10          	sub    $0x10,%rsp
  4025d7:	48 8b 17             	mov    (%rdi),%rdx
isort_540_807.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:533
    return (vec->upper - vec->lower);
  4025da:	48 8b 47 08          	mov    0x8(%rdi),%rax
  4025de:	48 29 d0             	sub    %rdx,%rax
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:239
    if (fltIf_1034_1137) {
  4025e1:	48 83 f8 01          	cmp    $0x1,%rax
  4025e5:	0f 8e f5 00 00 00    	jle    4026e0 <isort_540_807.part.0+0x120>
  4025eb:	49 89 f6             	mov    %rsi,%r14
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:244
        if (fltIf_1035_1138) {
  4025ee:	4d 85 ed             	test   %r13,%r13
  4025f1:	75 75                	jne    402668 <isort_540_807.part.0+0xa8>
isort_540_807.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4025f3:	48 83 c2 01          	add    $0x1,%rdx
  4025f7:	48 0f af 57 10       	imul   0x10(%rdi),%rdx
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:254
            GibInt fltAppE_1037_1143 = *tmp_4;
  4025fc:	48 8b 47 18          	mov    0x18(%rdi),%rax
isort_540_807.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402600:	48 8b 4e 18          	mov    0x18(%rsi),%rcx
  402604:	48 8b 3e             	mov    (%rsi),%rdi
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:254
  402607:	4c 8b 3c 10          	mov    (%rax,%rdx,1),%r15
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  40260b:	48 8b 56 10          	mov    0x10(%rsi),%rdx
  40260f:	4c 89 3c 24          	mov    %r15,(%rsp)
  402613:	49 8d 44 3d 00       	lea    0x0(%r13,%rdi,1),%rax
isort_540_807.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402618:	4c 01 e7             	add    %r12,%rdi
  40261b:	48 0f af c2          	imul   %rdx,%rax
  40261f:	48 0f af fa          	imul   %rdx,%rdi
insert_541_809():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:278
        GibInt y_72_947_1153 = *tmp_5;
  402623:	48 8b 04 01          	mov    (%rcx,%rax,1),%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402627:	48 01 cf             	add    %rcx,%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:278
  40262a:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:282
        if (fltIf_1041_1156) {
  40262f:	4c 39 f8             	cmp    %r15,%rax
  402632:	0f 8e 98 00 00 00    	jle    4026d0 <isort_540_807.part.0+0x110>
isort_540_807.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402638:	48 8d 74 24 08       	lea    0x8(%rsp),%rsi
  40263d:	e8 fe ea ff ff       	call   401140 <memcpy@plt>
insert_541_809():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:304
                       insert_541_809(xs__73_948_1163, x_69_945_1145, fltAppE_1043_1164);
  402642:	4c 89 f7             	mov    %r14,%rdi
  402645:	4c 89 ea             	mov    %r13,%rdx
  402648:	4c 89 fe             	mov    %r15,%rsi
  40264b:	e8 60 fc ff ff       	call   4022b0 <insert_541_809>
  402650:	49 89 c6             	mov    %rax,%r14
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:261
}
  402653:	48 83 c4 10          	add    $0x10,%rsp
  402657:	4c 89 f0             	mov    %r14,%rax
  40265a:	5b                   	pop    %rbx
  40265b:	41 5c                	pop    %r12
  40265d:	41 5d                	pop    %r13
  40265f:	41 5e                	pop    %r14
  402661:	41 5f                	pop    %r15
  402663:	c3                   	ret
  402664:	0f 1f 40 00          	nopl   0x0(%rax)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:244
        if (fltIf_1035_1138) {
  402668:	4d 89 e0             	mov    %r12,%r8
  40266b:	49 83 e8 02          	sub    $0x2,%r8
  40266f:	75 7f                	jne    4026f0 <isort_540_807.part.0+0x130>
isort_540_807.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402671:	4c 01 ea             	add    %r13,%rdx
  402674:	48 0f af 53 10       	imul   0x10(%rbx),%rdx
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:254
            GibInt fltAppE_1037_1143 = *tmp_4;
  402679:	48 8b 43 18          	mov    0x18(%rbx),%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:256
                       insert_541_809(xs__66_943_1140, fltAppE_1037_1143, n_63_941_1134);
  40267d:	4c 89 f7             	mov    %r14,%rdi
  402680:	48 8b 34 10          	mov    (%rax,%rdx,1),%rsi
  402684:	4c 89 ea             	mov    %r13,%rdx
  402687:	e8 24 fc ff ff       	call   4022b0 <insert_541_809>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  40268c:	48 8b 13             	mov    (%rbx),%rdx
isort_540_807.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  40268f:	48 8b 38             	mov    (%rax),%rdi
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:256
  402692:	49 89 c6             	mov    %rax,%r14
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402695:	4c 01 e2             	add    %r12,%rdx
  402698:	48 0f af 53 10       	imul   0x10(%rbx),%rdx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:254
            GibInt fltAppE_1037_1143 = *tmp_4;
  40269d:	48 8b 43 18          	mov    0x18(%rbx),%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4026a1:	49 8b 4e 18          	mov    0x18(%r14),%rcx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:254
  4026a5:	4c 8b 3c 10          	mov    (%rax,%rdx,1),%r15
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4026a9:	49 8b 56 10          	mov    0x10(%r14),%rdx
  4026ad:	4c 89 3c 24          	mov    %r15,(%rsp)
insert_541_809():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:267
    if (fltIf_1038_1147) {
  4026b1:	4d 85 e4             	test   %r12,%r12
  4026b4:	0f 85 59 ff ff ff    	jne    402613 <isort_540_807.part.0+0x53>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4026ba:	48 0f af fa          	imul   %rdx,%rdi
isort_540_807.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4026be:	48 89 e6             	mov    %rsp,%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4026c1:	48 01 cf             	add    %rcx,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4026c4:	e8 77 ea ff ff       	call   401140 <memcpy@plt>
  4026c9:	eb 88                	jmp    402653 <isort_540_807.part.0+0x93>
  4026cb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
  4026d0:	48 89 e6             	mov    %rsp,%rsi
  4026d3:	e8 68 ea ff ff       	call   401140 <memcpy@plt>
isort_540_807():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:258
            return tailapp_1222;
  4026d8:	e9 76 ff ff ff       	jmp    402653 <isort_540_807.part.0+0x93>
  4026dd:	0f 1f 00             	nopl   (%rax)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:239
    if (fltIf_1034_1137) {
  4026e0:	48 89 d7             	mov    %rdx,%rdi
  4026e3:	49 89 de             	mov    %rbx,%r14
  4026e6:	eb ad                	jmp    402695 <isort_540_807.part.0+0xd5>
  4026e8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  4026ef:	00 
  4026f0:	4c 89 c2             	mov    %r8,%rdx
  4026f3:	e8 c8 fe ff ff       	call   4025c0 <isort_540_807.part.0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4026f8:	48 8b 13             	mov    (%rbx),%rdx
  4026fb:	49 89 c6             	mov    %rax,%r14
  4026fe:	e9 6e ff ff ff       	jmp    402671 <isort_540_807.part.0+0xb1>
isort_540_807.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402703:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40270a:	00 00 00 00 
  40270e:	66 90                	xchg   %ax,%ax

0000000000402710 <shift_543_811.part.0>:
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:348
GibVector *shift_543_811(GibInt j_74_955_1180, GibVector *ys_76_956_1181)
  402710:	41 54                	push   %r12
  402712:	49 89 fc             	mov    %rdi,%r12
  402715:	55                   	push   %rbp
  402716:	48 89 f5             	mov    %rsi,%rbp
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:360
        GibInt i_98_883_1010_1187 = j_74_955_1180 - 1;
  402719:	49 83 ec 01          	sub    $0x1,%r12
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:348
GibVector *shift_543_811(GibInt j_74_955_1180, GibVector *ys_76_956_1181)
  40271d:	48 83 ec 18          	sub    $0x18,%rsp
shift_543_811.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402721:	48 8b 45 00          	mov    0x0(%rbp),%rax
  402725:	48 8b 55 10          	mov    0x10(%rbp),%rdx
  402729:	48 8b 76 18          	mov    0x18(%rsi),%rsi
  40272d:	48 01 c7             	add    %rax,%rdi
  402730:	4c 01 e0             	add    %r12,%rax
  402733:	48 0f af fa          	imul   %rdx,%rdi
  402737:	48 0f af c2          	imul   %rdx,%rax
  40273b:	48 01 f7             	add    %rsi,%rdi
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:359
        GibInt a_78_957_1185 = *tmp_8;
  40273e:	48 8b 0f             	mov    (%rdi),%rcx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:365
        GibInt b_79_958_1188 = *tmp_7;
  402741:	48 8b 04 06          	mov    (%rsi,%rax,1),%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:359
        GibInt a_78_957_1185 = *tmp_8;
  402745:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:365
        GibInt b_79_958_1188 = *tmp_7;
  40274a:	48 89 04 24          	mov    %rax,(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:376
            if (fltIf_1052_1192) {
  40274e:	48 39 c1             	cmp    %rax,%rcx
  402751:	7f 2a                	jg     40277d <shift_543_811.part.0+0x6d>
shift_543_811.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402753:	48 89 e6             	mov    %rsp,%rsi
  402756:	e8 e5 e9 ff ff       	call   401140 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  40275b:	48 8b 7d 00          	mov    0x0(%rbp),%rdi
  40275f:	48 8b 55 10          	mov    0x10(%rbp),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402763:	48 8d 74 24 08       	lea    0x8(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402768:	4c 01 e7             	add    %r12,%rdi
  40276b:	48 0f af fa          	imul   %rdx,%rdi
  40276f:	48 03 7d 18          	add    0x18(%rbp),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  402773:	e8 c8 e9 ff ff       	call   401140 <memcpy@plt>
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:352
    if (fltIf_1048_1182) {
  402778:	4d 85 e4             	test   %r12,%r12
  40277b:	75 13                	jne    402790 <shift_543_811.part.0+0x80>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:403
}
  40277d:	48 83 c4 18          	add    $0x18,%rsp
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:348
GibVector *shift_543_811(GibInt j_74_955_1180, GibVector *ys_76_956_1181)
  402781:	48 89 e8             	mov    %rbp,%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:403
}
  402784:	5d                   	pop    %rbp
  402785:	41 5c                	pop    %r12
  402787:	c3                   	ret
  402788:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40278f:	00 
  402790:	48 89 ee             	mov    %rbp,%rsi
  402793:	4c 89 e7             	mov    %r12,%rdi
  402796:	e8 75 ff ff ff       	call   402710 <shift_543_811.part.0>
  40279b:	48 83 c4 18          	add    $0x18,%rsp
  40279f:	5d                   	pop    %rbp
  4027a0:	41 5c                	pop    %r12
  4027a2:	c3                   	ret
shift_543_811.part.0():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:403
  4027a3:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  4027aa:	00 00 00 00 
  4027ae:	66 90                	xchg   %ax,%ax

00000000004027b0 <go_538_810.part.0.isra.0>:
go_538_810():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:331
GibVector *go_538_810(GibInt i_82_951_1174, GibInt n_83_952_1175,
  4027b0:	41 54                	push   %r12
  4027b2:	55                   	push   %rbp
  4027b3:	48 89 f5             	mov    %rsi,%rbp
  4027b6:	48 89 d6             	mov    %rdx,%rsi
  4027b9:	53                   	push   %rbx
  4027ba:	48 89 fb             	mov    %rdi,%rbx
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:352
    if (fltIf_1048_1182) {
  4027bd:	48 85 ff             	test   %rdi,%rdi
  4027c0:	75 1e                	jne    4027e0 <go_538_810.part.0.isra.0+0x30>
go_538_810():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
    if (fltIf_1046_1177) {
  4027c2:	48 83 fd 01          	cmp    $0x1,%rbp
  4027c6:	74 13                	je     4027db <go_538_810.part.0.isra.0+0x2b>
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
  4027c8:	bf 01 00 00 00       	mov    $0x1,%edi
  4027cd:	e8 3e ff ff ff       	call   402710 <shift_543_811.part.0>
  4027d2:	48 89 c6             	mov    %rax,%rsi
go_538_810():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
  4027d5:	48 83 fd 02          	cmp    $0x2,%rbp
  4027d9:	75 4e                	jne    402829 <go_538_810.part.0.isra.0+0x79>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:347
}
  4027db:	5b                   	pop    %rbx
  4027dc:	5d                   	pop    %rbp
  4027dd:	41 5c                	pop    %r12
  4027df:	c3                   	ret
  4027e0:	4c 8d 67 01          	lea    0x1(%rdi),%r12
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:347
  4027e4:	e8 27 ff ff ff       	call   402710 <shift_543_811.part.0>
  4027e9:	48 89 c6             	mov    %rax,%rsi
go_538_810():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
    if (fltIf_1046_1177) {
  4027ec:	4c 39 e5             	cmp    %r12,%rbp
  4027ef:	74 ea                	je     4027db <go_538_810.part.0.isra.0+0x2b>
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:352
    if (fltIf_1048_1182) {
  4027f1:	4d 85 e4             	test   %r12,%r12
  4027f4:	75 7b                	jne    402871 <go_538_810.part.0.isra.0+0xc1>
go_538_810():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
    if (fltIf_1046_1177) {
  4027f6:	48 83 fd 01          	cmp    $0x1,%rbp
  4027fa:	74 df                	je     4027db <go_538_810.part.0.isra.0+0x2b>
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
  4027fc:	bf 01 00 00 00       	mov    $0x1,%edi
  402801:	e8 0a ff ff ff       	call   402710 <shift_543_811.part.0>
go_538_810():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
  402806:	48 83 fd 02          	cmp    $0x2,%rbp
  40280a:	74 cf                	je     4027db <go_538_810.part.0.isra.0+0x2b>
  40280c:	bf 02 00 00 00       	mov    $0x2,%edi
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
  402811:	48 89 c6             	mov    %rax,%rsi
  402814:	e8 f7 fe ff ff       	call   402710 <shift_543_811.part.0>
go_538_810():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:341
        GibInt fltAppE_1047_1179 = i_82_951_1174 + 1;
  402819:	48 8d 7b 04          	lea    0x4(%rbx),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
    if (fltIf_1046_1177) {
  40281d:	48 39 fd             	cmp    %rdi,%rbp
  402820:	74 b9                	je     4027db <go_538_810.part.0.isra.0+0x2b>
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:352
    if (fltIf_1048_1182) {
  402822:	48 85 ff             	test   %rdi,%rdi
  402825:	74 2d                	je     402854 <go_538_810.part.0.isra.0+0xa4>
  402827:	eb 23                	jmp    40284c <go_538_810.part.0.isra.0+0x9c>
go_538_810():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
    if (fltIf_1046_1177) {
  402829:	bf 02 00 00 00       	mov    $0x2,%edi
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
  40282e:	e8 dd fe ff ff       	call   402710 <shift_543_811.part.0>
go_538_810():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:341
        GibInt fltAppE_1047_1179 = i_82_951_1174 + 1;
  402833:	48 8d 7b 03          	lea    0x3(%rbx),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
    if (fltIf_1046_1177) {
  402837:	48 39 fd             	cmp    %rdi,%rbp
  40283a:	74 9f                	je     4027db <go_538_810.part.0.isra.0+0x2b>
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:352
    if (fltIf_1048_1182) {
  40283c:	48 85 ff             	test   %rdi,%rdi
  40283f:	75 d0                	jne    402811 <go_538_810.part.0.isra.0+0x61>
go_538_810():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:341
        GibInt fltAppE_1047_1179 = i_82_951_1174 + 1;
  402841:	bf 01 00 00 00       	mov    $0x1,%edi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
    if (fltIf_1046_1177) {
  402846:	48 83 fd 01          	cmp    $0x1,%rbp
  40284a:	74 8f                	je     4027db <go_538_810.part.0.isra.0+0x2b>
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
  40284c:	48 89 c6             	mov    %rax,%rsi
  40284f:	e8 bc fe ff ff       	call   402710 <shift_543_811.part.0>
go_538_810():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:341
        GibInt fltAppE_1047_1179 = i_82_951_1174 + 1;
  402854:	48 8d 7b 05          	lea    0x5(%rbx),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
    if (fltIf_1046_1177) {
  402858:	48 39 fd             	cmp    %rdi,%rbp
  40285b:	0f 84 7a ff ff ff    	je     4027db <go_538_810.part.0.isra.0+0x2b>
  402861:	48 89 ee             	mov    %rbp,%rsi
  402864:	48 89 c2             	mov    %rax,%rdx
  402867:	e8 44 ff ff ff       	call   4027b0 <go_538_810.part.0.isra.0>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:347
}
  40286c:	5b                   	pop    %rbx
  40286d:	5d                   	pop    %rbp
  40286e:	41 5c                	pop    %r12
  402870:	c3                   	ret
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:347
  402871:	4c 89 e7             	mov    %r12,%rdi
  402874:	e8 97 fe ff ff       	call   402710 <shift_543_811.part.0>
go_538_810():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:341
        GibInt fltAppE_1047_1179 = i_82_951_1174 + 1;
  402879:	48 8d 7b 02          	lea    0x2(%rbx),%rdi
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:341
  40287d:	48 89 c6             	mov    %rax,%rsi
go_538_810():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
    if (fltIf_1046_1177) {
  402880:	48 39 fd             	cmp    %rdi,%rbp
  402883:	0f 84 52 ff ff ff    	je     4027db <go_538_810.part.0.isra.0+0x2b>
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:352
    if (fltIf_1048_1182) {
  402889:	48 85 ff             	test   %rdi,%rdi
  40288c:	75 a0                	jne    40282e <go_538_810.part.0.isra.0+0x7e>
go_538_810():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
    if (fltIf_1046_1177) {
  40288e:	48 83 fd 01          	cmp    $0x1,%rbp
  402892:	0f 84 43 ff ff ff    	je     4027db <go_538_810.part.0.isra.0+0x2b>
shift_543_811():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
  402898:	bf 01 00 00 00       	mov    $0x1,%edi
  40289d:	e8 6e fe ff ff       	call   402710 <shift_543_811.part.0>
go_538_810():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
  4028a2:	48 83 fd 02          	cmp    $0x2,%rbp
  4028a6:	0f 84 2f ff ff ff    	je     4027db <go_538_810.part.0.isra.0+0x2b>
  4028ac:	bf 02 00 00 00       	mov    $0x2,%edi
  4028b1:	eb 99                	jmp    40284c <go_538_810.part.0.isra.0+0x9c>
go_538_810.part.0.isra.0():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:336
  4028b3:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  4028ba:	00 00 00 00 
  4028be:	66 90                	xchg   %ax,%ax

00000000004028c0 <gib_vector_alloc.constprop.0>:
gib_vector_alloc.constprop.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:512
GibVector *gib_vector_alloc(GibInt num, size_t elt_size)
  4028c0:	41 54                	push   %r12
  4028c2:	53                   	push   %rbx
  4028c3:	48 89 fb             	mov    %rdi,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4028c6:	bf 20 00 00 00       	mov    $0x20,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:512
GibVector *gib_vector_alloc(GibInt num, size_t elt_size)
  4028cb:	48 83 ec 08          	sub    $0x8,%rsp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4028cf:	e8 8c e8 ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:515
    if (vec == NULL) {
  4028d4:	48 85 c0             	test   %rax,%rax
  4028d7:	74 3b                	je     402914 <gib_vector_alloc.constprop.0+0x54>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:519
    void *data = (void *) gib_alloc(num * elt_size);
  4028d9:	48 8d 3c dd 00 00 00 	lea    0x0(,%rbx,8),%rdi
  4028e0:	00 
  4028e1:	49 89 c4             	mov    %rax,%r12
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4028e4:	e8 77 e8 ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:520
    if (data == NULL) {
  4028e9:	48 85 c0             	test   %rax,%rax
  4028ec:	74 48                	je     402936 <gib_vector_alloc.constprop.0+0x76>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:527
    vec->data = data;
  4028ee:	49 89 44 24 18       	mov    %rax,0x18(%r12)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:529
}
  4028f3:	4c 89 e0             	mov    %r12,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:524
    vec->lower = 0;
  4028f6:	49 c7 04 24 00 00 00 	movq   $0x0,(%r12)
  4028fd:	00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:525
    vec->upper = num;
  4028fe:	49 89 5c 24 08       	mov    %rbx,0x8(%r12)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:526
    vec->elt_size = elt_size;
  402903:	49 c7 44 24 10 08 00 	movq   $0x8,0x10(%r12)
  40290a:	00 00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:529
}
  40290c:	48 83 c4 08          	add    $0x8,%rsp
  402910:	5b                   	pop    %rbx
  402911:	41 5c                	pop    %r12
  402913:	c3                   	ret
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:516
        fprintf(stderr, "alloc_vector: gib_alloc failed: %zu", sizeof(GibVector));
  402914:	48 8b 3d 25 38 00 00 	mov    0x3825(%rip),%rdi        # 406140 <stderr@GLIBC_2.2.5>
  40291b:	ba 20 00 00 00       	mov    $0x20,%edx
  402920:	be 08 40 40 00       	mov    $0x404008,%esi
  402925:	31 c0                	xor    %eax,%eax
  402927:	e8 04 e8 ff ff       	call   401130 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:517
        exit(1);
  40292c:	bf 01 00 00 00       	mov    $0x1,%edi
  402931:	e8 7a e8 ff ff       	call   4011b0 <exit@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:521
        fprintf(stderr, "alloc_vector: gib_alloc failed: %zu", sizeof(num * elt_size));
  402936:	48 8b 3d 03 38 00 00 	mov    0x3803(%rip),%rdi        # 406140 <stderr@GLIBC_2.2.5>
  40293d:	ba 08 00 00 00       	mov    $0x8,%edx
  402942:	be 08 40 40 00       	mov    $0x404008,%esi
  402947:	e8 e4 e7 ff ff       	call   401130 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:522
        exit(1);
  40294c:	bf 01 00 00 00       	mov    $0x1,%edi
  402951:	e8 5a e8 ff ff       	call   4011b0 <exit@plt>
  402956:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40295d:	00 00 00 

0000000000402960 <gib_shadowstack_initialize.part.0.constprop.0>:
gib_shadowstack_initialize.part.0.constprop.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1483
static void gib_shadowstack_initialize(GibShadowstack* stack, size_t stack_size)
  402960:	48 83 ec 08          	sub    $0x8,%rsp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1487
        fprintf(stderr, "gib_shadowstack_initialize: gib_alloc failed: %zu",
  402964:	ba 00 00 00 06       	mov    $0x6000000,%edx
  402969:	be 30 40 40 00       	mov    $0x404030,%esi
  40296e:	31 c0                	xor    %eax,%eax
  402970:	48 8b 3d c9 37 00 00 	mov    0x37c9(%rip),%rdi        # 406140 <stderr@GLIBC_2.2.5>
  402977:	e8 b4 e7 ff ff       	call   401130 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1489
        exit(1);
  40297c:	bf 01 00 00 00       	mov    $0x1,%edi
  402981:	e8 2a e8 ff ff       	call   4011b0 <exit@plt>
  402986:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40298d:	00 00 00 

0000000000402990 <gib_print_timing_array>:
gib_print_timing_array():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:673
void gib_print_timing_array(GibVector *times) {
  402990:	41 55                	push   %r13
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:674
    printf("ITER TIMES: [");
  402992:	31 c0                	xor    %eax,%eax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:673
void gib_print_timing_array(GibVector *times) {
  402994:	49 89 fd             	mov    %rdi,%r13
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:674
    printf("ITER TIMES: [");
  402997:	bf 03 47 40 00       	mov    $0x404703,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:673
void gib_print_timing_array(GibVector *times) {
  40299c:	41 54                	push   %r12
  40299e:	55                   	push   %rbp
  40299f:	53                   	push   %rbx
  4029a0:	48 83 ec 08          	sub    $0x8,%rsp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:674
    printf("ITER TIMES: [");
  4029a4:	e8 17 e7 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:533
    return (vec->upper - vec->lower);
  4029a9:	49 8b 45 00          	mov    0x0(%r13),%rax
  4029ad:	49 8b 6d 08          	mov    0x8(%r13),%rbp
  4029b1:	48 29 c5             	sub    %rax,%rbp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:677
    for(GibInt i = 0; i < n; i++) {
  4029b4:	48 85 ed             	test   %rbp,%rbp
  4029b7:	7e 59                	jle    402a12 <gib_print_timing_array+0x82>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:679
        if (i == (n-1)) {
  4029b9:	4c 8d 65 ff          	lea    -0x1(%rbp),%r12
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:677
    for(GibInt i = 0; i < n; i++) {
  4029bd:	31 db                	xor    %ebx,%ebx
  4029bf:	eb 23                	jmp    4029e4 <gib_print_timing_array+0x54>
  4029c1:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:683
            printf("%f, ",*d);
  4029c8:	bf 14 47 40 00       	mov    $0x404714,%edi
  4029cd:	b8 01 00 00 00       	mov    $0x1,%eax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:677
    for(GibInt i = 0; i < n; i++) {
  4029d2:	48 83 c3 01          	add    $0x1,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:683
            printf("%f, ",*d);
  4029d6:	e8 e5 e6 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:677
    for(GibInt i = 0; i < n; i++) {
  4029db:	48 39 eb             	cmp    %rbp,%rbx
  4029de:	74 32                	je     402a12 <gib_print_timing_array+0x82>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4029e0:	49 8b 45 00          	mov    0x0(%r13),%rax
  4029e4:	48 01 d8             	add    %rbx,%rax
  4029e7:	49 0f af 45 10       	imul   0x10(%r13),%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:680
            printf("%f",*d);
  4029ec:	49 8b 55 18          	mov    0x18(%r13),%rdx
  4029f0:	f2 0f 10 04 02       	movsd  (%rdx,%rax,1),%xmm0
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:679
        if (i == (n-1)) {
  4029f5:	4c 39 e3             	cmp    %r12,%rbx
  4029f8:	75 ce                	jne    4029c8 <gib_print_timing_array+0x38>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:680
            printf("%f",*d);
  4029fa:	bf 11 47 40 00       	mov    $0x404711,%edi
  4029ff:	b8 01 00 00 00       	mov    $0x1,%eax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:677
    for(GibInt i = 0; i < n; i++) {
  402a04:	48 83 c3 01          	add    $0x1,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:680
            printf("%f",*d);
  402a08:	e8 b3 e6 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:677
    for(GibInt i = 0; i < n; i++) {
  402a0d:	48 39 dd             	cmp    %rbx,%rbp
  402a10:	75 ce                	jne    4029e0 <gib_print_timing_array+0x50>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:688
}
  402a12:	48 83 c4 08          	add    $0x8,%rsp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:686
    printf("]\n");
  402a16:	bf 19 47 40 00       	mov    $0x404719,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:688
}
  402a1b:	5b                   	pop    %rbx
  402a1c:	5d                   	pop    %rbp
  402a1d:	41 5c                	pop    %r12
  402a1f:	41 5d                	pop    %r13
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:686
    printf("]\n");
  402a21:	e9 5a e6 ff ff       	jmp    401080 <puts@plt>
  402a26:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  402a2d:	00 00 00 

0000000000402a30 <gib_show_usage>:
gib_show_usage():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1797
{
  402a30:	53                   	push   %rbx
  402a31:	48 89 fb             	mov    %rdi,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1798
    printf("\n");
  402a34:	bf 0a 00 00 00       	mov    $0xa,%edi
  402a39:	e8 02 e6 ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1799
    printf("This binary was generated by the Gibbon compiler.\n");
  402a3e:	bf 68 40 40 00       	mov    $0x404068,%edi
  402a43:	e8 38 e6 ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1800
    printf("\n");
  402a48:	bf 0a 00 00 00       	mov    $0xa,%edi
  402a4d:	e8 ee e5 ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1801
    printf("Usage: %s [OPTIONS...]\n", argv[0]);
  402a52:	48 8b 33             	mov    (%rbx),%rsi
  402a55:	bf 1b 47 40 00       	mov    $0x40471b,%edi
  402a5a:	31 c0                	xor    %eax,%eax
  402a5c:	e8 5f e6 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1803
    printf("\n");
  402a61:	bf 0a 00 00 00       	mov    $0xa,%edi
  402a66:	e8 d5 e5 ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1804
    printf("Options:\n");
  402a6b:	bf 33 47 40 00       	mov    $0x404733,%edi
  402a70:	e8 0b e6 ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1805
    printf(" --biginf-buffer-size <bytes>   Set the buffer size (default %" PRId64 ").\n", gib_global_biginf_init_chunk_size);
  402a75:	48 8b 35 74 36 00 00 	mov    0x3674(%rip),%rsi        # 4060f0 <gib_global_biginf_init_chunk_size>
  402a7c:	bf a0 40 40 00       	mov    $0x4040a0,%edi
  402a81:	31 c0                	xor    %eax,%eax
  402a83:	e8 38 e6 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1806
    printf(" --inf-buffer-size <bytes>      Set the buffer size (default %" PRId64 ").\n", gib_global_inf_init_chunk_size);
  402a88:	48 8b 35 59 36 00 00 	mov    0x3659(%rip),%rsi        # 4060e8 <gib_global_inf_init_chunk_size>
  402a8f:	bf e8 40 40 00       	mov    $0x4040e8,%edi
  402a94:	31 c0                	xor    %eax,%eax
  402a96:	e8 25 e6 ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1807
    printf(" --bench-input <path>           Set the input file read for benchmarking. Applies only\n");
  402a9b:	bf 30 41 40 00       	mov    $0x404130,%edi
  402aa0:	e8 db e5 ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1808
    printf("                                If the program was *compiled* with --bench-fun. \n");
  402aa5:	bf 88 41 40 00       	mov    $0x404188,%edi
  402aaa:	e8 d1 e5 ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1809
    printf("\n");
  402aaf:	bf 0a 00 00 00       	mov    $0xa,%edi
  402ab4:	e8 87 e5 ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1810
    printf(" --array-input <path>           Set the file from which to read the array input.\n");
  402ab9:	bf e0 41 40 00       	mov    $0x4041e0,%edi
  402abe:	e8 bd e5 ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1811
    printf(" --array-input-length <int>     Set the size of the array input file.\n");
  402ac3:	bf 38 42 40 00       	mov    $0x404238,%edi
  402ac8:	e8 b3 e5 ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1812
    printf(" --iterate <int>                Set the number of timing iterations to perform (default 1).\n");
  402acd:	bf 80 42 40 00       	mov    $0x404280,%edi
  402ad2:	e8 a9 e5 ff ff       	call   401080 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1814
    printf(" --size-param <int>             A parameter for size available as a language primitive which allows user to specify the size at runtime (default 1).\n");
  402ad7:	bf e0 42 40 00       	mov    $0x4042e0,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1816
}
  402adc:	5b                   	pop    %rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1814
    printf(" --size-param <int>             A parameter for size available as a language primitive which allows user to specify the size at runtime (default 1).\n");
  402add:	e9 9e e5 ff ff       	jmp    401080 <puts@plt>
  402ae2:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  402ae9:	00 00 00 00 
  402aed:	0f 1f 00             	nopl   (%rax)

0000000000402af0 <check_args.part.0>:
check_args.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1876
void check_args(int i, int argc, char **argv, char *parameter){
  402af0:	55                   	push   %rbp
  402af1:	48 89 fd             	mov    %rdi,%rbp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1878
        fprintf(stderr, "Not enough arguments after %s, expected <int>.\n", parameter);
  402af4:	48 8b 3d 45 36 00 00 	mov    0x3645(%rip),%rdi        # 406140 <stderr@GLIBC_2.2.5>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1876
void check_args(int i, int argc, char **argv, char *parameter){
  402afb:	48 89 f2             	mov    %rsi,%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1878
        fprintf(stderr, "Not enough arguments after %s, expected <int>.\n", parameter);
  402afe:	31 c0                	xor    %eax,%eax
  402b00:	be 78 43 40 00       	mov    $0x404378,%esi
  402b05:	e8 26 e6 ff ff       	call   401130 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1879
        gib_show_usage(argv);
  402b0a:	48 89 ef             	mov    %rbp,%rdi
  402b0d:	e8 1e ff ff ff       	call   402a30 <gib_show_usage>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1880
        exit(1);
  402b12:	bf 01 00 00 00       	mov    $0x1,%edi
  402b17:	e8 94 e6 ff ff       	call   4011b0 <exit@plt>
  402b1c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402b20 <gib_add_symbol>:
gib_add_symbol():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:411
{
  402b20:	41 56                	push   %r14
  402b22:	41 55                	push   %r13
  402b24:	41 54                	push   %r12
  402b26:	49 89 fc             	mov    %rdi,%r12
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  402b29:	bf 40 01 00 00       	mov    $0x140,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:411
{
  402b2e:	55                   	push   %rbp
  402b2f:	48 89 f5             	mov    %rsi,%rbp
  402b32:	53                   	push   %rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  402b33:	e8 28 e6 ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:415
    strcpy(s->value, value);
  402b38:	48 89 ee             	mov    %rbp,%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:414
    s->idx = idx;
  402b3b:	4c 89 20             	mov    %r12,(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:415
    strcpy(s->value, value);
  402b3e:	48 8d 78 08          	lea    0x8(%rax),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  402b42:	48 89 c3             	mov    %rax,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:415
    strcpy(s->value, value);
  402b45:	e8 16 e5 ff ff       	call   401060 <strcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416
    HASH_ADD(hh, global_sym_table, idx, sizeof(GibSym), s);
  402b4a:	4c 89 e0             	mov    %r12,%rax
  402b4d:	4c 89 e1             	mov    %r12,%rcx
  402b50:	48 89 9b 30 01 00 00 	mov    %rbx,0x130(%rbx)
  402b57:	48 c1 e8 38          	shr    $0x38,%rax
  402b5b:	48 c1 e9 30          	shr    $0x30,%rcx
  402b5f:	c7 83 38 01 00 00 08 	movl   $0x8,0x138(%rbx)
  402b66:	00 00 00 
  402b69:	48 89 c2             	mov    %rax,%rdx
  402b6c:	4c 89 e0             	mov    %r12,%rax
  402b6f:	0f b6 c9             	movzbl %cl,%ecx
  402b72:	48 c1 e8 20          	shr    $0x20,%rax
  402b76:	c1 e2 18             	shl    $0x18,%edx
  402b79:	0f b6 c0             	movzbl %al,%eax
  402b7c:	c1 e1 10             	shl    $0x10,%ecx
  402b7f:	8d 94 02 b9 79 37 9e 	lea    -0x61c88647(%rdx,%rax,1),%edx
  402b86:	8d 04 0a             	lea    (%rdx,%rcx,1),%eax
  402b89:	4c 89 e1             	mov    %r12,%rcx
  402b8c:	44 89 e2             	mov    %r12d,%edx
  402b8f:	48 c1 e9 28          	shr    $0x28,%rcx
  402b93:	81 e2 ff ff 00 ff    	and    $0xff00ffff,%edx
  402b99:	0f b6 c9             	movzbl %cl,%ecx
  402b9c:	c1 e1 08             	shl    $0x8,%ecx
  402b9f:	01 c1                	add    %eax,%ecx
  402ba1:	4c 89 e0             	mov    %r12,%rax
  402ba4:	48 c1 e8 10          	shr    $0x10,%rax
  402ba8:	0f b6 c0             	movzbl %al,%eax
  402bab:	c1 e0 10             	shl    $0x10,%eax
  402bae:	8d 84 02 c2 ba 49 9f 	lea    -0x60b6453e(%rdx,%rax,1),%eax
  402bb5:	29 c8                	sub    %ecx,%eax
  402bb7:	81 c1 09 41 12 01    	add    $0x1124109,%ecx
  402bbd:	35 6d f7 07 00       	xor    $0x7f76d,%eax
  402bc2:	29 c1                	sub    %eax,%ecx
  402bc4:	89 ca                	mov    %ecx,%edx
  402bc6:	89 c1                	mov    %eax,%ecx
  402bc8:	c1 e1 08             	shl    $0x8,%ecx
  402bcb:	31 d1                	xor    %edx,%ecx
  402bcd:	ba f7 be ed fe       	mov    $0xfeedbef7,%edx
  402bd2:	29 c2                	sub    %eax,%edx
  402bd4:	29 c8                	sub    %ecx,%eax
  402bd6:	89 d6                	mov    %edx,%esi
  402bd8:	89 ca                	mov    %ecx,%edx
  402bda:	29 ce                	sub    %ecx,%esi
  402bdc:	c1 ea 0d             	shr    $0xd,%edx
  402bdf:	31 f2                	xor    %esi,%edx
  402be1:	89 d6                	mov    %edx,%esi
  402be3:	29 d0                	sub    %edx,%eax
  402be5:	29 d1                	sub    %edx,%ecx
  402be7:	c1 ee 0c             	shr    $0xc,%esi
  402bea:	31 f0                	xor    %esi,%eax
  402bec:	89 ce                	mov    %ecx,%esi
  402bee:	89 c1                	mov    %eax,%ecx
  402bf0:	29 c6                	sub    %eax,%esi
  402bf2:	29 c2                	sub    %eax,%edx
  402bf4:	c1 e1 10             	shl    $0x10,%ecx
  402bf7:	31 f1                	xor    %esi,%ecx
  402bf9:	89 d6                	mov    %edx,%esi
  402bfb:	89 ca                	mov    %ecx,%edx
  402bfd:	29 ce                	sub    %ecx,%esi
  402bff:	29 c8                	sub    %ecx,%eax
  402c01:	c1 ea 05             	shr    $0x5,%edx
  402c04:	31 f2                	xor    %esi,%edx
  402c06:	89 d6                	mov    %edx,%esi
  402c08:	29 d0                	sub    %edx,%eax
  402c0a:	29 d1                	sub    %edx,%ecx
  402c0c:	c1 ee 03             	shr    $0x3,%esi
  402c0f:	31 f0                	xor    %esi,%eax
  402c11:	89 c5                	mov    %eax,%ebp
  402c13:	29 c1                	sub    %eax,%ecx
  402c15:	29 c2                	sub    %eax,%edx
  402c17:	48 8b 05 3a 35 00 00 	mov    0x353a(%rip),%rax        # 406158 <global_sym_table>
  402c1e:	c1 e5 0a             	shl    $0xa,%ebp
  402c21:	31 e9                	xor    %ebp,%ecx
  402c23:	29 ca                	sub    %ecx,%edx
  402c25:	c1 e9 0f             	shr    $0xf,%ecx
  402c28:	31 d1                	xor    %edx,%ecx
  402c2a:	89 8b 3c 01 00 00    	mov    %ecx,0x13c(%rbx)
  402c30:	89 cd                	mov    %ecx,%ebp
  402c32:	48 85 c0             	test   %rax,%rax
  402c35:	0f 84 a7 01 00 00    	je     402de2 <gib_add_symbol+0x2c2>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 29)
  402c3b:	48 8b 90 08 01 00 00 	mov    0x108(%rax),%rdx
  402c42:	4c 8d ab 08 01 00 00 	lea    0x108(%rbx),%r13
  402c49:	48 c7 83 18 01 00 00 	movq   $0x0,0x118(%rbx)
  402c50:	00 00 00 00 
  402c54:	48 89 93 08 01 00 00 	mov    %rdx,0x108(%rbx)
  402c5b:	48 8b 90 08 01 00 00 	mov    0x108(%rax),%rdx
  402c62:	48 8b 4a 18          	mov    0x18(%rdx),%rcx
  402c66:	48 89 ce             	mov    %rcx,%rsi
  402c69:	48 2b 72 20          	sub    0x20(%rdx),%rsi
  402c6d:	48 89 b3 10 01 00 00 	mov    %rsi,0x110(%rbx)
  402c74:	48 89 59 10          	mov    %rbx,0x10(%rcx)
  402c78:	4c 89 6a 18          	mov    %r13,0x18(%rdx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 37)
  402c7c:	48 8b 90 08 01 00 00 	mov    0x108(%rax),%rdx
  402c83:	8b 42 08             	mov    0x8(%rdx),%eax
  402c86:	83 42 10 01          	addl   $0x1,0x10(%rdx)
  402c8a:	83 e8 01             	sub    $0x1,%eax
  402c8d:	21 e8                	and    %ebp,%eax
  402c8f:	48 c1 e0 04          	shl    $0x4,%rax
  402c93:	48 03 02             	add    (%rdx),%rax
  402c96:	8b 78 08             	mov    0x8(%rax),%edi
  402c99:	48 8b 08             	mov    (%rax),%rcx
  402c9c:	8d 57 01             	lea    0x1(%rdi),%edx
  402c9f:	89 50 08             	mov    %edx,0x8(%rax)
  402ca2:	48 89 8b 28 01 00 00 	mov    %rcx,0x128(%rbx)
  402ca9:	48 c7 83 20 01 00 00 	movq   $0x0,0x120(%rbx)
  402cb0:	00 00 00 00 
  402cb4:	48 85 c9             	test   %rcx,%rcx
  402cb7:	74 04                	je     402cbd <gib_add_symbol+0x19d>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 38)
  402cb9:	4c 89 69 18          	mov    %r13,0x18(%rcx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 40)
  402cbd:	4c 89 28             	mov    %r13,(%rax)
  402cc0:	8b 40 0c             	mov    0xc(%rax),%eax
  402cc3:	8d 44 80 05          	lea    0x5(%rax,%rax,4),%eax
  402cc7:	01 c0                	add    %eax,%eax
  402cc9:	39 c2                	cmp    %eax,%edx
  402ccb:	72 0d                	jb     402cda <gib_add_symbol+0x1ba>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 41)
  402ccd:	48 8b ab 08 01 00 00 	mov    0x108(%rbx),%rbp
  402cd4:	83 7d 34 00          	cmpl   $0x0,0x34(%rbp)
  402cd8:	74 19                	je     402cf3 <gib_add_symbol+0x1d3>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:417
    if (idx > gib_global_gensym_counter) {
  402cda:	4c 39 25 6f 34 00 00 	cmp    %r12,0x346f(%rip)        # 406150 <gib_global_gensym_counter>
  402ce1:	73 07                	jae    402cea <gib_add_symbol+0x1ca>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:418
        gib_global_gensym_counter = idx;
  402ce3:	4c 89 25 66 34 00 00 	mov    %r12,0x3466(%rip)        # 406150 <gib_global_gensym_counter>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:421
}
  402cea:	5b                   	pop    %rbx
  402ceb:	5d                   	pop    %rbp
  402cec:	41 5c                	pop    %r12
  402cee:	41 5d                	pop    %r13
  402cf0:	41 5e                	pop    %r14
  402cf2:	c3                   	ret
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 42)
    HASH_ADD(hh, global_sym_table, idx, sizeof(GibSym), s);
  402cf3:	8b 7d 08             	mov    0x8(%rbp),%edi
  402cf6:	be 01 00 00 00       	mov    $0x1,%esi
  402cfb:	48 c1 e7 05          	shl    $0x5,%rdi
  402cff:	e8 fc e3 ff ff       	call   401100 <calloc@plt>
  402d04:	49 89 c5             	mov    %rax,%r13
  402d07:	48 85 c0             	test   %rax,%rax
  402d0a:	0f 84 a4 01 00 00    	je     402eb4 <gib_add_symbol+0x394>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 44)
  402d10:	8b 55 08             	mov    0x8(%rbp),%edx
  402d13:	8b 45 10             	mov    0x10(%rbp),%eax
  402d16:	c7 45 2c 00 00 00 00 	movl   $0x0,0x2c(%rbp)
  402d1d:	8b 7d 0c             	mov    0xc(%rbp),%edi
  402d20:	44 8d 44 12 ff       	lea    -0x1(%rdx,%rdx,1),%r8d
  402d25:	8d 4f 01             	lea    0x1(%rdi),%ecx
  402d28:	89 c7                	mov    %eax,%edi
  402d2a:	44 21 c0             	and    %r8d,%eax
  402d2d:	d3 ef                	shr    %cl,%edi
  402d2f:	83 f8 01             	cmp    $0x1,%eax
  402d32:	83 df ff             	sbb    $0xffffffff,%edi
  402d35:	89 7d 28             	mov    %edi,0x28(%rbp)
  402d38:	85 d2                	test   %edx,%edx
  402d3a:	0f 84 26 01 00 00    	je     402e66 <gib_add_symbol+0x346>
  402d40:	48 8b 45 00          	mov    0x0(%rbp),%rax
  402d44:	44 8d 52 ff          	lea    -0x1(%rdx),%r10d
  402d48:	49 c1 e2 04          	shl    $0x4,%r10
  402d4c:	4c 8d 48 10          	lea    0x10(%rax),%r9
  402d50:	4d 01 ca             	add    %r9,%r10
  402d53:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 63)
  402d58:	48 8b 08             	mov    (%rax),%rcx
  402d5b:	48 85 c9             	test   %rcx,%rcx
  402d5e:	75 17                	jne    402d77 <gib_add_symbol+0x257>
  402d60:	eb 6b                	jmp    402dcd <gib_add_symbol+0x2ad>
  402d62:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 56)
  402d68:	48 89 4a 18          	mov    %rcx,0x18(%rdx)
  402d6c:	48 89 08             	mov    %rcx,(%rax)
  402d6f:	48 85 f6             	test   %rsi,%rsi
  402d72:	74 59                	je     402dcd <gib_add_symbol+0x2ad>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416
  402d74:	48 89 f1             	mov    %rsi,%rcx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 60)
  402d77:	44 89 c0             	mov    %r8d,%eax
  402d7a:	23 41 34             	and    0x34(%rcx),%eax
  402d7d:	48 8b 71 20          	mov    0x20(%rcx),%rsi
  402d81:	48 c1 e0 04          	shl    $0x4,%rax
  402d85:	4c 01 e8             	add    %r13,%rax
  402d88:	8b 50 08             	mov    0x8(%rax),%edx
  402d8b:	83 c2 01             	add    $0x1,%edx
  402d8e:	89 50 08             	mov    %edx,0x8(%rax)
  402d91:	39 d7                	cmp    %edx,%edi
  402d93:	73 1c                	jae    402db1 <gib_add_symbol+0x291>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 51)
  402d95:	44 8b 58 0c          	mov    0xc(%rax),%r11d
  402d99:	41 89 fe             	mov    %edi,%r14d
  402d9c:	83 45 2c 01          	addl   $0x1,0x2c(%rbp)
  402da0:	45 0f af f3          	imul   %r11d,%r14d
  402da4:	44 39 f2             	cmp    %r14d,%edx
  402da7:	76 08                	jbe    402db1 <gib_add_symbol+0x291>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 53)
  402da9:	41 83 c3 01          	add    $0x1,%r11d
  402dad:	44 89 58 0c          	mov    %r11d,0xc(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 55)
  402db1:	48 8b 10             	mov    (%rax),%rdx
  402db4:	48 c7 41 18 00 00 00 	movq   $0x0,0x18(%rcx)
  402dbb:	00 
  402dbc:	48 89 51 20          	mov    %rdx,0x20(%rcx)
  402dc0:	48 85 d2             	test   %rdx,%rdx
  402dc3:	75 a3                	jne    402d68 <gib_add_symbol+0x248>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 58)
  402dc5:	48 89 08             	mov    %rcx,(%rax)
  402dc8:	48 85 f6             	test   %rsi,%rsi
  402dcb:	75 a7                	jne    402d74 <gib_add_symbol+0x254>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416
  402dcd:	4c 89 c8             	mov    %r9,%rax
  402dd0:	4d 39 d1             	cmp    %r10,%r9
  402dd3:	0f 84 8d 00 00 00    	je     402e66 <gib_add_symbol+0x346>
  402dd9:	49 83 c1 10          	add    $0x10,%r9
  402ddd:	e9 76 ff ff ff       	jmp    402d58 <gib_add_symbol+0x238>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 28)
  402de2:	66 0f ef c0          	pxor   %xmm0,%xmm0
  402de6:	bf 40 00 00 00       	mov    $0x40,%edi
  402deb:	0f 11 83 10 01 00 00 	movups %xmm0,0x110(%rbx)
  402df2:	e8 69 e3 ff ff       	call   401160 <malloc@plt>
  402df7:	48 89 83 08 01 00 00 	mov    %rax,0x108(%rbx)
  402dfe:	49 89 c6             	mov    %rax,%r14
  402e01:	48 85 c0             	test   %rax,%rax
  402e04:	0f 84 aa 00 00 00    	je     402eb4 <gib_add_symbol+0x394>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 31)
  402e0a:	48 8d 50 10          	lea    0x10(%rax),%rdx
  402e0e:	b9 0c 00 00 00       	mov    $0xc,%ecx
  402e13:	31 c0                	xor    %eax,%eax
  402e15:	be 01 00 00 00       	mov    $0x1,%esi
  402e1a:	48 89 d7             	mov    %rdx,%rdi
  402e1d:	4c 8d ab 08 01 00 00 	lea    0x108(%rbx),%r13
  402e24:	f3 ab                	rep stos %eax,%es:(%rdi)
  402e26:	48 8b 05 c3 1a 00 00 	mov    0x1ac3(%rip),%rax        # 4048f0 <__PRETTY_FUNCTION__.3+0x20>
  402e2d:	4d 89 6e 18          	mov    %r13,0x18(%r14)
  402e31:	bf 00 02 00 00       	mov    $0x200,%edi
  402e36:	49 c7 46 20 08 01 00 	movq   $0x108,0x20(%r14)
  402e3d:	00 
  402e3e:	49 89 46 08          	mov    %rax,0x8(%r14)
  402e42:	e8 b9 e2 ff ff       	call   401100 <calloc@plt>
  402e47:	41 c7 46 38 e1 1f 11 	movl   $0xa0111fe1,0x38(%r14)
  402e4e:	a0 
  402e4f:	49 89 06             	mov    %rax,(%r14)
  402e52:	48 85 c0             	test   %rax,%rax
  402e55:	74 5d                	je     402eb4 <gib_add_symbol+0x394>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 33)
  402e57:	48 89 1d fa 32 00 00 	mov    %rbx,0x32fa(%rip)        # 406158 <global_sym_table>
  402e5e:	48 89 d8             	mov    %rbx,%rax
  402e61:	e9 16 fe ff ff       	jmp    402c7c <gib_add_symbol+0x15c>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416
  402e66:	48 8b 7d 00          	mov    0x0(%rbp),%rdi
  402e6a:	e8 c1 e1 ff ff       	call   401030 <free@plt>
  402e6f:	48 8b 83 08 01 00 00 	mov    0x108(%rbx),%rax
  402e76:	8b 50 10             	mov    0x10(%rax),%edx
  402e79:	83 40 0c 01          	addl   $0x1,0xc(%rax)
  402e7d:	4c 89 28             	mov    %r13,(%rax)
  402e80:	d1 60 08             	shll   $1,0x8(%rax)
  402e83:	d1 ea                	shr    $1,%edx
  402e85:	39 50 2c             	cmp    %edx,0x2c(%rax)
  402e88:	76 1e                	jbe    402ea8 <gib_add_symbol+0x388>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 65)
  402e8a:	8b 58 30             	mov    0x30(%rax),%ebx
  402e8d:	8d 53 01             	lea    0x1(%rbx),%edx
  402e90:	89 50 30             	mov    %edx,0x30(%rax)
  402e93:	83 fa 01             	cmp    $0x1,%edx
  402e96:	0f 86 3e fe ff ff    	jbe    402cda <gib_add_symbol+0x1ba>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 69)
  402e9c:	c7 40 34 01 00 00 00 	movl   $0x1,0x34(%rax)
  402ea3:	e9 32 fe ff ff       	jmp    402cda <gib_add_symbol+0x1ba>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416
  402ea8:	c7 40 30 00 00 00 00 	movl   $0x0,0x30(%rax)
  402eaf:	e9 26 fe ff ff       	jmp    402cda <gib_add_symbol+0x1ba>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:416 (discriminator 30)
  402eb4:	83 cf ff             	or     $0xffffffff,%edi
  402eb7:	e8 f4 e2 ff ff       	call   4011b0 <exit@plt>
  402ebc:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402ec0 <gib_check_rust_struct_sizes>:
gib_check_rust_struct_sizes():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:994
{
  402ec0:	55                   	push   %rbp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  402ec1:	bf 38 00 00 00       	mov    $0x38,%edi
  402ec6:	e8 95 e2 ff ff       	call   401160 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1004
    gib_get_rust_struct_sizes(stack, frame, nursery, generation, reg_info, footer, gc_stats);
  402ecb:	48 83 ec 08          	sub    $0x8,%rsp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  402ecf:	48 89 c5             	mov    %rax,%rbp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:999
    nursery = (size_t *) ((char *) frame + sizeof(size_t));
  402ed2:	48 8d 50 10          	lea    0x10(%rax),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1000
    generation = (size_t *) ((char *) nursery + sizeof(size_t));
  402ed6:	48 8d 48 18          	lea    0x18(%rax),%rcx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:998
    frame = (size_t *) ((char *) stack + sizeof(size_t));
  402eda:	48 8d 70 08          	lea    0x8(%rax),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1003
    gc_stats = (size_t *) ((char *) footer + sizeof(size_t));
  402ede:	48 83 c0 30          	add    $0x30,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1004
    gib_get_rust_struct_sizes(stack, frame, nursery, generation, reg_info, footer, gc_stats);
  402ee2:	4c 8d 4d 28          	lea    0x28(%rbp),%r9
  402ee6:	48 89 ef             	mov    %rbp,%rdi
  402ee9:	50                   	push   %rax
  402eea:	4c 8d 45 20          	lea    0x20(%rbp),%r8
  402eee:	e8 9d e2 ff ff       	call   401190 <gib_get_rust_struct_sizes@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1007
    assert(*stack == sizeof(GibShadowstack));
  402ef3:	48 83 7d 00 18       	cmpq   $0x18,0x0(%rbp)
  402ef8:	58                   	pop    %rax
  402ef9:	5a                   	pop    %rdx
  402efa:	75 42                	jne    402f3e <gib_check_rust_struct_sizes+0x7e>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1008
    assert(*frame == sizeof(GibShadowstackFrame));
  402efc:	48 83 7d 08 18       	cmpq   $0x18,0x8(%rbp)
  402f01:	0f 85 cd 00 00 00    	jne    402fd4 <gib_check_rust_struct_sizes+0x114>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1009
    assert(*nursery == sizeof(GibNursery));
  402f07:	48 83 7d 10 20       	cmpq   $0x20,0x10(%rbp)
  402f0c:	0f 85 a9 00 00 00    	jne    402fbb <gib_check_rust_struct_sizes+0xfb>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1010
    assert(*generation == sizeof(GibOldgen));
  402f12:	48 83 7d 18 18       	cmpq   $0x18,0x18(%rbp)
  402f17:	0f 85 85 00 00 00    	jne    402fa2 <gib_check_rust_struct_sizes+0xe2>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1011
    assert(*reg_info == sizeof(GibRegionInfo));
  402f1d:	48 83 7d 20 20       	cmpq   $0x20,0x20(%rbp)
  402f22:	75 65                	jne    402f89 <gib_check_rust_struct_sizes+0xc9>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1012
    assert(*footer == sizeof(GibOldgenChunkFooter));
  402f24:	48 83 7d 28 18       	cmpq   $0x18,0x28(%rbp)
  402f29:	75 45                	jne    402f70 <gib_check_rust_struct_sizes+0xb0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1013
    assert(*gc_stats == sizeof(GibGcStats));
  402f2b:	48 81 7d 30 f0 00 00 	cmpq   $0xf0,0x30(%rbp)
  402f32:	00 
  402f33:	75 22                	jne    402f57 <gib_check_rust_struct_sizes+0x97>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:242
void gib_free(void *ptr) { free(ptr); }
  402f35:	48 89 ef             	mov    %rbp,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1019
}
  402f38:	5d                   	pop    %rbp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:242
void gib_free(void *ptr) { free(ptr); }
  402f39:	e9 f2 e0 ff ff       	jmp    401030 <free@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1007 (discriminator 1)
    assert(*stack == sizeof(GibShadowstack));
  402f3e:	b9 d0 48 40 00       	mov    $0x4048d0,%ecx
  402f43:	ba ef 03 00 00       	mov    $0x3ef,%edx
  402f48:	be a8 43 40 00       	mov    $0x4043a8,%esi
  402f4d:	bf e8 43 40 00       	mov    $0x4043e8,%edi
  402f52:	e8 89 e1 ff ff       	call   4010e0 <__assert_fail@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1013 (discriminator 1)
    assert(*gc_stats == sizeof(GibGcStats));
  402f57:	b9 d0 48 40 00       	mov    $0x4048d0,%ecx
  402f5c:	ba f5 03 00 00       	mov    $0x3f5,%edx
  402f61:	be a8 43 40 00       	mov    $0x4043a8,%esi
  402f66:	bf d0 44 40 00       	mov    $0x4044d0,%edi
  402f6b:	e8 70 e1 ff ff       	call   4010e0 <__assert_fail@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1012 (discriminator 1)
    assert(*footer == sizeof(GibOldgenChunkFooter));
  402f70:	b9 d0 48 40 00       	mov    $0x4048d0,%ecx
  402f75:	ba f4 03 00 00       	mov    $0x3f4,%edx
  402f7a:	be a8 43 40 00       	mov    $0x4043a8,%esi
  402f7f:	bf a8 44 40 00       	mov    $0x4044a8,%edi
  402f84:	e8 57 e1 ff ff       	call   4010e0 <__assert_fail@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1011 (discriminator 1)
    assert(*reg_info == sizeof(GibRegionInfo));
  402f89:	b9 d0 48 40 00       	mov    $0x4048d0,%ecx
  402f8e:	ba f3 03 00 00       	mov    $0x3f3,%edx
  402f93:	be a8 43 40 00       	mov    $0x4043a8,%esi
  402f98:	bf 80 44 40 00       	mov    $0x404480,%edi
  402f9d:	e8 3e e1 ff ff       	call   4010e0 <__assert_fail@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1010 (discriminator 1)
    assert(*generation == sizeof(GibOldgen));
  402fa2:	b9 d0 48 40 00       	mov    $0x4048d0,%ecx
  402fa7:	ba f2 03 00 00       	mov    $0x3f2,%edx
  402fac:	be a8 43 40 00       	mov    $0x4043a8,%esi
  402fb1:	bf 58 44 40 00       	mov    $0x404458,%edi
  402fb6:	e8 25 e1 ff ff       	call   4010e0 <__assert_fail@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1009 (discriminator 1)
    assert(*nursery == sizeof(GibNursery));
  402fbb:	b9 d0 48 40 00       	mov    $0x4048d0,%ecx
  402fc0:	ba f1 03 00 00       	mov    $0x3f1,%edx
  402fc5:	be a8 43 40 00       	mov    $0x4043a8,%esi
  402fca:	bf 38 44 40 00       	mov    $0x404438,%edi
  402fcf:	e8 0c e1 ff ff       	call   4010e0 <__assert_fail@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1008 (discriminator 1)
    assert(*frame == sizeof(GibShadowstackFrame));
  402fd4:	b9 d0 48 40 00       	mov    $0x4048d0,%ecx
  402fd9:	ba f0 03 00 00       	mov    $0x3f0,%edx
  402fde:	be a8 43 40 00       	mov    $0x4043a8,%esi
  402fe3:	bf 10 44 40 00       	mov    $0x404410,%edi
  402fe8:	e8 f3 e0 ff ff       	call   4010e0 <__assert_fail@plt>
  402fed:	0f 1f 00             	nopl   (%rax)

0000000000402ff0 <gib_print_symbol.isra.0>:
gib_print_symbol.isra.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:458
int gib_print_symbol(GibSym idx)
  402ff0:	48 83 ec 18          	sub    $0x18,%rsp
  402ff4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:460
    if (idx == comma_symbol) {
  402ff9:	48 83 ff ff          	cmp    $0xffffffffffffffff,%rdi
  402ffd:	0f 84 65 01 00 00    	je     403168 <gib_print_symbol.isra.0+0x178>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:472
        HASH_FIND(hh, global_sym_table, &idx, sizeof(GibSym), s);
  403003:	48 8b 35 4e 31 00 00 	mov    0x314e(%rip),%rsi        # 406158 <global_sym_table>
  40300a:	49 89 f8             	mov    %rdi,%r8
  40300d:	48 85 f6             	test   %rsi,%rsi
  403010:	0f 84 3a 01 00 00    	je     403150 <gib_print_symbol.isra.0+0x160>
  403016:	0f b6 44 24 0f       	movzbl 0xf(%rsp),%eax
  40301b:	0f b6 54 24 0c       	movzbl 0xc(%rsp),%edx
  403020:	0f b6 7c 24 0e       	movzbl 0xe(%rsp),%edi
  403025:	c1 e0 18             	shl    $0x18,%eax
  403028:	c1 e7 10             	shl    $0x10,%edi
  40302b:	8d 94 10 b9 79 37 9e 	lea    -0x61c88647(%rax,%rdx,1),%edx
  403032:	8d 04 3a             	lea    (%rdx,%rdi,1),%eax
  403035:	0f b6 7c 24 0d       	movzbl 0xd(%rsp),%edi
  40303a:	0f b6 54 24 08       	movzbl 0x8(%rsp),%edx
  40303f:	c1 e7 08             	shl    $0x8,%edi
  403042:	01 c7                	add    %eax,%edi
  403044:	0f b6 44 24 0b       	movzbl 0xb(%rsp),%eax
  403049:	c1 e0 18             	shl    $0x18,%eax
  40304c:	8d 8c 02 c2 ba 49 9f 	lea    -0x60b6453e(%rdx,%rax,1),%ecx
  403053:	0f b6 44 24 0a       	movzbl 0xa(%rsp),%eax
  403058:	c1 e0 10             	shl    $0x10,%eax
  40305b:	8d 14 01             	lea    (%rcx,%rax,1),%edx
  40305e:	0f b6 44 24 09       	movzbl 0x9(%rsp),%eax
  403063:	b9 f7 be ed fe       	mov    $0xfeedbef7,%ecx
  403068:	c1 e0 08             	shl    $0x8,%eax
  40306b:	01 d0                	add    %edx,%eax
  40306d:	29 f8                	sub    %edi,%eax
  40306f:	81 c7 09 41 12 01    	add    $0x1124109,%edi
  403075:	35 6d f7 07 00       	xor    $0x7f76d,%eax
  40307a:	89 c2                	mov    %eax,%edx
  40307c:	29 c7                	sub    %eax,%edi
  40307e:	29 c1                	sub    %eax,%ecx
  403080:	c1 e2 08             	shl    $0x8,%edx
  403083:	31 fa                	xor    %edi,%edx
  403085:	29 d1                	sub    %edx,%ecx
  403087:	29 d0                	sub    %edx,%eax
  403089:	89 cf                	mov    %ecx,%edi
  40308b:	89 d1                	mov    %edx,%ecx
  40308d:	c1 e9 0d             	shr    $0xd,%ecx
  403090:	31 f9                	xor    %edi,%ecx
  403092:	89 cf                	mov    %ecx,%edi
  403094:	29 c8                	sub    %ecx,%eax
  403096:	29 ca                	sub    %ecx,%edx
  403098:	c1 ef 0c             	shr    $0xc,%edi
  40309b:	31 f8                	xor    %edi,%eax
  40309d:	89 d7                	mov    %edx,%edi
  40309f:	89 c2                	mov    %eax,%edx
  4030a1:	29 c7                	sub    %eax,%edi
  4030a3:	29 c1                	sub    %eax,%ecx
  4030a5:	c1 e2 10             	shl    $0x10,%edx
  4030a8:	31 fa                	xor    %edi,%edx
  4030aa:	29 d1                	sub    %edx,%ecx
  4030ac:	29 d0                	sub    %edx,%eax
  4030ae:	89 cf                	mov    %ecx,%edi
  4030b0:	89 d1                	mov    %edx,%ecx
  4030b2:	c1 e9 05             	shr    $0x5,%ecx
  4030b5:	31 f9                	xor    %edi,%ecx
  4030b7:	89 cf                	mov    %ecx,%edi
  4030b9:	29 c8                	sub    %ecx,%eax
  4030bb:	c1 ef 03             	shr    $0x3,%edi
  4030be:	29 ca                	sub    %ecx,%edx
  4030c0:	31 c7                	xor    %eax,%edi
  4030c2:	89 d0                	mov    %edx,%eax
  4030c4:	89 fa                	mov    %edi,%edx
  4030c6:	29 f8                	sub    %edi,%eax
  4030c8:	c1 e2 0a             	shl    $0xa,%edx
  4030cb:	31 c2                	xor    %eax,%edx
  4030cd:	89 c8                	mov    %ecx,%eax
  4030cf:	48 8b 8e 08 01 00 00 	mov    0x108(%rsi),%rcx
  4030d6:	29 f8                	sub    %edi,%eax
  4030d8:	8b 79 08             	mov    0x8(%rcx),%edi
  4030db:	29 d0                	sub    %edx,%eax
  4030dd:	c1 ea 0f             	shr    $0xf,%edx
  4030e0:	31 d0                	xor    %edx,%eax
  4030e2:	8d 57 ff             	lea    -0x1(%rdi),%edx
  4030e5:	21 c2                	and    %eax,%edx
  4030e7:	48 c1 e2 04          	shl    $0x4,%rdx
  4030eb:	48 03 11             	add    (%rcx),%rdx
  4030ee:	48 8b 32             	mov    (%rdx),%rsi
  4030f1:	48 85 f6             	test   %rsi,%rsi
  4030f4:	74 5a                	je     403150 <gib_print_symbol.isra.0+0x160>
  4030f6:	48 8b 49 20          	mov    0x20(%rcx),%rcx
  4030fa:	48 89 ca             	mov    %rcx,%rdx
  4030fd:	48 29 ce             	sub    %rcx,%rsi
  403100:	48 f7 da             	neg    %rdx
  403103:	eb 12                	jmp    403117 <gib_print_symbol.isra.0+0x127>
  403105:	0f 1f 00             	nopl   (%rax)
  403108:	48 8b b6 28 01 00 00 	mov    0x128(%rsi),%rsi
  40310f:	48 85 f6             	test   %rsi,%rsi
  403112:	74 3c                	je     403150 <gib_print_symbol.isra.0+0x160>
  403114:	48 01 d6             	add    %rdx,%rsi
  403117:	3b 86 3c 01 00 00    	cmp    0x13c(%rsi),%eax
  40311d:	75 e9                	jne    403108 <gib_print_symbol.isra.0+0x118>
  40311f:	83 be 38 01 00 00 08 	cmpl   $0x8,0x138(%rsi)
  403126:	75 e0                	jne    403108 <gib_print_symbol.isra.0+0x118>
  403128:	48 8b 8e 30 01 00 00 	mov    0x130(%rsi),%rcx
  40312f:	48 8b 09             	mov    (%rcx),%rcx
  403132:	48 39 4c 24 08       	cmp    %rcx,0x8(%rsp)
  403137:	75 cf                	jne    403108 <gib_print_symbol.isra.0+0x118>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:476
            return printf("%s", s->value);
  403139:	48 83 c6 08          	add    $0x8,%rsi
  40313d:	bf 40 47 40 00       	mov    $0x404740,%edi
  403142:	31 c0                	xor    %eax,%eax
  403144:	e8 77 df ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:480
}
  403149:	48 83 c4 18          	add    $0x18,%rsp
  40314d:	c3                   	ret
  40314e:	66 90                	xchg   %ax,%ax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:474
            return printf("%" PRId64, idx);
  403150:	4c 89 c6             	mov    %r8,%rsi
  403153:	bf 3c 47 40 00       	mov    $0x40473c,%edi
  403158:	31 c0                	xor    %eax,%eax
  40315a:	e8 61 df ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:480
}
  40315f:	48 83 c4 18          	add    $0x18,%rsp
  403163:	c3                   	ret
  403164:	0f 1f 40 00          	nopl   0x0(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:461
        return printf(",");
  403168:	bf 2c 00 00 00       	mov    $0x2c,%edi
  40316d:	e8 ce de ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:480
}
  403172:	48 83 c4 18          	add    $0x18,%rsp
  403176:	c3                   	ret
  403177:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40317e:	00 00 

0000000000403180 <printVec_loop_545_802.part.0.isra.0>:
printVec_loop_545_802():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:124
unsigned char printVec_loop_545_802(GibInt idx_160_908_1079,
  403180:	41 54                	push   %r12
  403182:	49 89 d4             	mov    %rdx,%r12
  403185:	55                   	push   %rbp
  403186:	48 89 f5             	mov    %rsi,%rbp
  403189:	53                   	push   %rbx
  40318a:	48 89 fb             	mov    %rdi,%rbx
  40318d:	48 83 ec 10          	sub    $0x10,%rsp
printVec_loop_545_802.part.0.isra.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  403191:	48 8b 02             	mov    (%rdx),%rax
  403194:	48 01 f8             	add    %rdi,%rax
  403197:	48 0f af 42 10       	imul   0x10(%rdx),%rax
printVec_loop_545_802():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:137
        GibInt i_47_891_973_1083 = *tmp_0;
  40319c:	48 8b 52 18          	mov    0x18(%rdx),%rdx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:138
        unsigned char wildcard__187_165_911_1084 = printf("%ld",
  4031a0:	bf 3c 47 40 00       	mov    $0x40473c,%edi
  4031a5:	48 8b 34 02          	mov    (%rdx,%rax,1),%rsi
  4031a9:	31 c0                	xor    %eax,%eax
  4031ab:	e8 10 df ff ff       	call   4010c0 <printf@plt>
printVec_loop_545_802.part.0.isra.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:472
        HASH_FIND(hh, global_sym_table, &idx, sizeof(GibSym), s);
  4031b0:	48 8b 05 a1 2f 00 00 	mov    0x2fa1(%rip),%rax        # 406158 <global_sym_table>
  4031b7:	48 c7 44 24 08 d6 04 	movq   $0x4d6,0x8(%rsp)
  4031be:	00 00 
  4031c0:	48 85 c0             	test   %rax,%rax
  4031c3:	0f 84 97 00 00 00    	je     403260 <printVec_loop_545_802.part.0.isra.0+0xe0>
  4031c9:	48 8b 90 08 01 00 00 	mov    0x108(%rax),%rdx
  4031d0:	8b 42 08             	mov    0x8(%rdx),%eax
  4031d3:	83 e8 01             	sub    $0x1,%eax
  4031d6:	25 75 05 08 4d       	and    $0x4d080575,%eax
  4031db:	48 c1 e0 04          	shl    $0x4,%rax
  4031df:	48 03 02             	add    (%rdx),%rax
  4031e2:	48 8b 30             	mov    (%rax),%rsi
  4031e5:	48 85 f6             	test   %rsi,%rsi
  4031e8:	74 76                	je     403260 <printVec_loop_545_802.part.0.isra.0+0xe0>
  4031ea:	48 8b 52 20          	mov    0x20(%rdx),%rdx
  4031ee:	48 89 d0             	mov    %rdx,%rax
  4031f1:	48 29 d6             	sub    %rdx,%rsi
  4031f4:	48 f7 d8             	neg    %rax
  4031f7:	eb 16                	jmp    40320f <printVec_loop_545_802.part.0.isra.0+0x8f>
  4031f9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)
  403200:	48 8b b6 28 01 00 00 	mov    0x128(%rsi),%rsi
  403207:	48 85 f6             	test   %rsi,%rsi
  40320a:	74 54                	je     403260 <printVec_loop_545_802.part.0.isra.0+0xe0>
  40320c:	48 01 c6             	add    %rax,%rsi
  40320f:	81 be 3c 01 00 00 75 	cmpl   $0x4d080575,0x13c(%rsi)
  403216:	05 08 4d 
  403219:	75 e5                	jne    403200 <printVec_loop_545_802.part.0.isra.0+0x80>
  40321b:	83 be 38 01 00 00 08 	cmpl   $0x8,0x138(%rsi)
  403222:	75 dc                	jne    403200 <printVec_loop_545_802.part.0.isra.0+0x80>
  403224:	48 8b 96 30 01 00 00 	mov    0x130(%rsi),%rdx
  40322b:	48 8b 0a             	mov    (%rdx),%rcx
  40322e:	48 39 4c 24 08       	cmp    %rcx,0x8(%rsp)
  403233:	75 cb                	jne    403200 <printVec_loop_545_802.part.0.isra.0+0x80>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:476
            return printf("%s", s->value);
  403235:	48 83 c6 08          	add    $0x8,%rsi
  403239:	bf 40 47 40 00       	mov    $0x404740,%edi
  40323e:	31 c0                	xor    %eax,%eax
  403240:	e8 7b de ff ff       	call   4010c0 <printf@plt>
printVec_loop_545_802():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:141
        GibInt fltAppE_1022_1086 = idx_160_908_1079 + 1;
  403245:	48 8d 43 01          	lea    0x1(%rbx),%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:130
    if (fltIf_1021_1082) {
  403249:	48 39 e8             	cmp    %rbp,%rax
  40324c:	75 2c                	jne    40327a <printVec_loop_545_802.part.0.isra.0+0xfa>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:147
}
  40324e:	48 83 c4 10          	add    $0x10,%rsp
  403252:	5b                   	pop    %rbx
  403253:	5d                   	pop    %rbp
  403254:	41 5c                	pop    %r12
  403256:	c3                   	ret
  403257:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40325e:	00 00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:474
            return printf("%" PRId64, idx);
  403260:	be d6 04 00 00       	mov    $0x4d6,%esi
  403265:	bf 3c 47 40 00       	mov    $0x40473c,%edi
  40326a:	31 c0                	xor    %eax,%eax
  40326c:	e8 4f de ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:141
        GibInt fltAppE_1022_1086 = idx_160_908_1079 + 1;
  403271:	48 8d 43 01          	lea    0x1(%rbx),%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:130
    if (fltIf_1021_1082) {
  403275:	48 39 e8             	cmp    %rbp,%rax
  403278:	74 d4                	je     40324e <printVec_loop_545_802.part.0.isra.0+0xce>
printVec_loop_545_802.part.0.isra.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  40327a:	49 03 04 24          	add    (%r12),%rax
  40327e:	49 0f af 44 24 10    	imul   0x10(%r12),%rax
printVec_loop_545_802():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:138
        unsigned char wildcard__187_165_911_1084 = printf("%ld",
  403284:	bf 3c 47 40 00       	mov    $0x40473c,%edi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:137
        GibInt i_47_891_973_1083 = *tmp_0;
  403289:	49 8b 54 24 18       	mov    0x18(%r12),%rdx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:138
        unsigned char wildcard__187_165_911_1084 = printf("%ld",
  40328e:	48 8b 34 02          	mov    (%rdx,%rax,1),%rsi
  403292:	31 c0                	xor    %eax,%eax
  403294:	e8 27 de ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:140
        unsigned char wildcard__184_166_912_1085 = gib_print_symbol(1238);
  403299:	bf d6 04 00 00       	mov    $0x4d6,%edi
  40329e:	e8 4d fd ff ff       	call   402ff0 <gib_print_symbol.isra.0>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:141
        GibInt fltAppE_1022_1086 = idx_160_908_1079 + 1;
  4032a3:	48 8d 43 02          	lea    0x2(%rbx),%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:130
    if (fltIf_1021_1082) {
  4032a7:	48 39 c5             	cmp    %rax,%rbp
  4032aa:	74 a2                	je     40324e <printVec_loop_545_802.part.0.isra.0+0xce>
printVec_loop_545_802.part.0.isra.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4032ac:	49 03 04 24          	add    (%r12),%rax
  4032b0:	49 0f af 44 24 10    	imul   0x10(%r12),%rax
printVec_loop_545_802():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:138
        unsigned char wildcard__187_165_911_1084 = printf("%ld",
  4032b6:	bf 3c 47 40 00       	mov    $0x40473c,%edi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:137
        GibInt i_47_891_973_1083 = *tmp_0;
  4032bb:	49 8b 54 24 18       	mov    0x18(%r12),%rdx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:138
        unsigned char wildcard__187_165_911_1084 = printf("%ld",
  4032c0:	48 8b 34 02          	mov    (%rdx,%rax,1),%rsi
  4032c4:	31 c0                	xor    %eax,%eax
  4032c6:	e8 f5 dd ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:140
        unsigned char wildcard__184_166_912_1085 = gib_print_symbol(1238);
  4032cb:	bf d6 04 00 00       	mov    $0x4d6,%edi
  4032d0:	e8 1b fd ff ff       	call   402ff0 <gib_print_symbol.isra.0>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:141
        GibInt fltAppE_1022_1086 = idx_160_908_1079 + 1;
  4032d5:	48 8d 43 03          	lea    0x3(%rbx),%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:130
    if (fltIf_1021_1082) {
  4032d9:	48 39 c5             	cmp    %rax,%rbp
  4032dc:	0f 84 6c ff ff ff    	je     40324e <printVec_loop_545_802.part.0.isra.0+0xce>
printVec_loop_545_802.part.0.isra.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4032e2:	49 03 04 24          	add    (%r12),%rax
  4032e6:	49 0f af 44 24 10    	imul   0x10(%r12),%rax
printVec_loop_545_802():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:138
        unsigned char wildcard__187_165_911_1084 = printf("%ld",
  4032ec:	bf 3c 47 40 00       	mov    $0x40473c,%edi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:137
        GibInt i_47_891_973_1083 = *tmp_0;
  4032f1:	49 8b 54 24 18       	mov    0x18(%r12),%rdx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:138
        unsigned char wildcard__187_165_911_1084 = printf("%ld",
  4032f6:	48 8b 34 02          	mov    (%rdx,%rax,1),%rsi
  4032fa:	31 c0                	xor    %eax,%eax
  4032fc:	e8 bf dd ff ff       	call   4010c0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:140
        unsigned char wildcard__184_166_912_1085 = gib_print_symbol(1238);
  403301:	bf d6 04 00 00       	mov    $0x4d6,%edi
  403306:	e8 e5 fc ff ff       	call   402ff0 <gib_print_symbol.isra.0>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:141
        GibInt fltAppE_1022_1086 = idx_160_908_1079 + 1;
  40330b:	48 8d 7b 04          	lea    0x4(%rbx),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/Insertion.c:130
    if (fltIf_1021_1082) {
  40330f:	48 39 fd             	cmp    %rdi,%rbp
  403312:	0f 84 36 ff ff ff    	je     40324e <printVec_loop_545_802.part.0.isra.0+0xce>
  403318:	4c 89 e2             	mov    %r12,%rdx
  40331b:	48 89 ee             	mov    %rbp,%rsi
  40331e:	e8 5d fe ff ff       	call   403180 <printVec_loop_545_802.part.0.isra.0>
  403323:	e9 26 ff ff ff       	jmp    40324e <printVec_loop_545_802.part.0.isra.0+0xce>

Disassembly of section .fini:

0000000000403328 <_fini>:
_fini():
  403328:	f3 0f 1e fa          	endbr64
  40332c:	48 83 ec 08          	sub    $0x8,%rsp
  403330:	48 83 c4 08          	add    $0x8,%rsp
  403334:	c3                   	ret
