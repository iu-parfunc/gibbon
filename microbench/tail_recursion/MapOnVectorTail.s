
MapOnVectorTail.exe:     file format elf64-x86-64


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

0000000000401060 <gib_init_zcts@plt>:
  401060:	ff 25 b2 3f 00 00    	jmp    *0x3fb2(%rip)        # 405018 <gib_init_zcts@Base>
  401066:	68 03 00 00 00       	push   $0x3
  40106b:	e9 b0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401070 <puts@plt>:
  401070:	ff 25 aa 3f 00 00    	jmp    *0x3faa(%rip)        # 405020 <puts@GLIBC_2.2.5>
  401076:	68 04 00 00 00       	push   $0x4
  40107b:	e9 a0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401080 <qsort@plt>:
  401080:	ff 25 a2 3f 00 00    	jmp    *0x3fa2(%rip)        # 405028 <qsort@GLIBC_2.2.5>
  401086:	68 05 00 00 00       	push   $0x5
  40108b:	e9 90 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401090 <clock_gettime@plt>:
  401090:	ff 25 9a 3f 00 00    	jmp    *0x3f9a(%rip)        # 405030 <clock_gettime@GLIBC_2.17>
  401096:	68 06 00 00 00       	push   $0x6
  40109b:	e9 80 ff ff ff       	jmp    401020 <_init+0x20>

00000000004010a0 <strlen@plt>:
  4010a0:	ff 25 92 3f 00 00    	jmp    *0x3f92(%rip)        # 405038 <strlen@GLIBC_2.2.5>
  4010a6:	68 07 00 00 00       	push   $0x7
  4010ab:	e9 70 ff ff ff       	jmp    401020 <_init+0x20>

00000000004010b0 <printf@plt>:
  4010b0:	ff 25 8a 3f 00 00    	jmp    *0x3f8a(%rip)        # 405040 <printf@GLIBC_2.2.5>
  4010b6:	68 08 00 00 00       	push   $0x8
  4010bb:	e9 60 ff ff ff       	jmp    401020 <_init+0x20>

00000000004010c0 <gib_gc_cleanup@plt>:
  4010c0:	ff 25 82 3f 00 00    	jmp    *0x3f82(%rip)        # 405048 <gib_gc_cleanup@Base>
  4010c6:	68 09 00 00 00       	push   $0x9
  4010cb:	e9 50 ff ff ff       	jmp    401020 <_init+0x20>

00000000004010d0 <__assert_fail@plt>:
  4010d0:	ff 25 7a 3f 00 00    	jmp    *0x3f7a(%rip)        # 405050 <__assert_fail@GLIBC_2.2.5>
  4010d6:	68 0a 00 00 00       	push   $0xa
  4010db:	e9 40 ff ff ff       	jmp    401020 <_init+0x20>

00000000004010e0 <gib_info_table_initialize@plt>:
  4010e0:	ff 25 72 3f 00 00    	jmp    *0x3f72(%rip)        # 405058 <gib_info_table_initialize@Base>
  4010e6:	68 0b 00 00 00       	push   $0xb
  4010eb:	e9 30 ff ff ff       	jmp    401020 <_init+0x20>

00000000004010f0 <strcmp@plt>:
  4010f0:	ff 25 6a 3f 00 00    	jmp    *0x3f6a(%rip)        # 405060 <strcmp@GLIBC_2.2.5>
  4010f6:	68 0c 00 00 00       	push   $0xc
  4010fb:	e9 20 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401100 <strtoll@plt>:
  401100:	ff 25 62 3f 00 00    	jmp    *0x3f62(%rip)        # 405068 <strtoll@GLIBC_2.2.5>
  401106:	68 0d 00 00 00       	push   $0xd
  40110b:	e9 10 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401110 <fprintf@plt>:
  401110:	ff 25 5a 3f 00 00    	jmp    *0x3f5a(%rip)        # 405070 <fprintf@GLIBC_2.2.5>
  401116:	68 0e 00 00 00       	push   $0xe
  40111b:	e9 00 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401120 <memcpy@plt>:
  401120:	ff 25 52 3f 00 00    	jmp    *0x3f52(%rip)        # 405078 <memcpy@GLIBC_2.14>
  401126:	68 0f 00 00 00       	push   $0xf
  40112b:	e9 f0 fe ff ff       	jmp    401020 <_init+0x20>

0000000000401130 <malloc@plt>:
  401130:	ff 25 4a 3f 00 00    	jmp    *0x3f4a(%rip)        # 405080 <malloc@GLIBC_2.2.5>
  401136:	68 10 00 00 00       	push   $0x10
  40113b:	e9 e0 fe ff ff       	jmp    401020 <_init+0x20>

0000000000401140 <setrlimit@plt>:
  401140:	ff 25 42 3f 00 00    	jmp    *0x3f42(%rip)        # 405088 <setrlimit@GLIBC_2.2.5>
  401146:	68 11 00 00 00       	push   $0x11
  40114b:	e9 d0 fe ff ff       	jmp    401020 <_init+0x20>

0000000000401150 <gib_get_rust_struct_sizes@plt>:
  401150:	ff 25 3a 3f 00 00    	jmp    *0x3f3a(%rip)        # 405090 <gib_get_rust_struct_sizes@Base>
  401156:	68 12 00 00 00       	push   $0x12
  40115b:	e9 c0 fe ff ff       	jmp    401020 <_init+0x20>

0000000000401160 <gib_info_table_finalize@plt>:
  401160:	ff 25 32 3f 00 00    	jmp    *0x3f32(%rip)        # 405098 <gib_info_table_finalize@Base>
  401166:	68 13 00 00 00       	push   $0x13
  40116b:	e9 b0 fe ff ff       	jmp    401020 <_init+0x20>

0000000000401170 <exit@plt>:
  401170:	ff 25 2a 3f 00 00    	jmp    *0x3f2a(%rip)        # 4050a0 <exit@GLIBC_2.2.5>
  401176:	68 14 00 00 00       	push   $0x14
  40117b:	e9 a0 fe ff ff       	jmp    401020 <_init+0x20>

0000000000401180 <fwrite@plt>:
  401180:	ff 25 22 3f 00 00    	jmp    *0x3f22(%rip)        # 4050a8 <fwrite@GLIBC_2.2.5>
  401186:	68 15 00 00 00       	push   $0x15
  40118b:	e9 90 fe ff ff       	jmp    401020 <_init+0x20>

0000000000401190 <getrlimit@plt>:
  401190:	ff 25 1a 3f 00 00    	jmp    *0x3f1a(%rip)        # 4050b0 <getrlimit@GLIBC_2.2.5>
  401196:	68 16 00 00 00       	push   $0x16
  40119b:	e9 80 fe ff ff       	jmp    401020 <_init+0x20>

Disassembly of section .text:

00000000004011a0 <main>:
main():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:145
        
        return tailapp_669;
    }
}
int main(int argc, char **argv)
{
  4011a0:	41 57                	push   %r15
  4011a2:	41 56                	push   %r14
  4011a4:	41 55                	push   %r13
  4011a6:	41 54                	push   %r12
  4011a8:	55                   	push   %rbp
  4011a9:	53                   	push   %rbx
  4011aa:	89 fb                	mov    %edi,%ebx
  4011ac:	48 83 ec 58          	sub    $0x58,%rsp
  4011b0:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1900
    // Print the GC configuration.
    gib_print_gc_config();
#endif

    // Ensure that C and Rust agree on sizes of structs that cross the boundary.
    gib_check_rust_struct_sizes();
  4011b5:	e8 86 0f 00 00       	call   402140 <gib_check_rust_struct_sizes>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1909
    //   num iterations: How many times to repeat a benchmark.
    //   tree size: An integer passes to `build_tree()`.

    struct rlimit lim;
    int code;
    if ( (code = getrlimit(RLIMIT_STACK, &lim)) ) {
  4011ba:	48 8d 74 24 40       	lea    0x40(%rsp),%rsi
  4011bf:	bf 03 00 00 00       	mov    $0x3,%edi
  4011c4:	e8 c7 ff ff ff       	call   401190 <getrlimit@plt>
  4011c9:	89 44 24 04          	mov    %eax,0x4(%rsp)
  4011cd:	85 c0                	test   %eax,%eax
  4011cf:	0f 85 51 0a 00 00    	jne    401c26 <main+0xa86>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1914
        fprintf(stderr, " [gibbon rts] failed to getrlimit, code %d\n", code);
        exit(1);
    }

    lim.rlim_cur = 4 * 1024LU * 1024LU * 1024LU; // 1GB stack.
  4011d5:	48 b8 00 00 00 00 01 	movabs $0x100000000,%rax
  4011dc:	00 00 00 
  4011df:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4011e4:	eb 3b                	jmp    401221 <main+0x81>
  4011e6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4011ed:	00 00 00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1922

    // WARNING: Haven't yet figured out why this doesn't work on MacOS...
#ifndef __APPLE__
    code = setrlimit(RLIMIT_STACK, &lim);
    while (code) {
        fprintf(stderr, " [gibbon rts] Failed to set stack size to %lu, code %d\n",
  4011f0:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  4011f5:	48 8b 3d 04 3f 00 00 	mov    0x3f04(%rip),%rdi        # 405100 <stderr@GLIBC_2.2.5>
  4011fc:	be 20 35 40 00       	mov    $0x403520,%esi
  401201:	31 c0                	xor    %eax,%eax
  401203:	e8 08 ff ff ff       	call   401110 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1924
                (uint64_t)lim.rlim_cur, code);
        lim.rlim_cur /= 2;
  401208:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40120d:	48 d1 e8             	shr    $1,%rax
  401210:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1926
        // lim.rlim_max /= 2;
        if(lim.rlim_cur < 100 * 1024) {
  401215:	48 3d ff 8f 01 00    	cmp    $0x18fff,%rax
  40121b:	0f 86 ba 02 00 00    	jbe    4014db <main+0x33b>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1920
    code = setrlimit(RLIMIT_STACK, &lim);
  401221:	48 8d 74 24 40       	lea    0x40(%rsp),%rsi
  401226:	bf 03 00 00 00       	mov    $0x3,%edi
  40122b:	e8 10 ff ff ff       	call   401140 <setrlimit@plt>
  401230:	89 c1                	mov    %eax,%ecx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1921
    while (code) {
  401232:	85 c0                	test   %eax,%eax
  401234:	75 ba                	jne    4011f0 <main+0x50>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1937
#endif

    // int got_numargs = argc; // How many numeric arguments have we got.

    int i;
    for (i = 1; i < argc; ++i)
  401236:	83 fb 01             	cmp    $0x1,%ebx
  401239:	0f 8e bc 02 00 00    	jle    4014fb <main+0x35b>
  40123f:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  401244:	8d 6b fe             	lea    -0x2(%rbx),%ebp
  401247:	41 be 02 00 00 00    	mov    $0x2,%r14d
  40124d:	41 bf 01 00 00 00    	mov    $0x1,%r15d
  401253:	83 e5 fe             	and    $0xfffffffe,%ebp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1968
        else if (strcmp(argv[i], "--array-input-length") == 0 && i < argc - 1) {
            check_args(i, argc, argv, "--array-input-length");
            gib_global_arrayfile_length_param = atoll(argv[i+1]);
            i++;
        }
        else if (strcmp(argv[i], "--bench-prog") == 0 && i < argc - 1) {
  401256:	44 8d 63 ff          	lea    -0x1(%rbx),%r12d
  40125a:	4c 8d 68 10          	lea    0x10(%rax),%r13
  40125e:	83 c5 03             	add    $0x3,%ebp
  401261:	eb 43                	jmp    4012a6 <main+0x106>
  401263:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1943
        else if (strcmp(argv[i], "--biginf-buffer-size") == 0 && i < argc - 1) {
  401268:	45 39 fc             	cmp    %r15d,%r12d
  40126b:	0f 8e cf 00 00 00    	jle    401340 <main+0x1a0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  401271:	44 39 f3             	cmp    %r14d,%ebx
  401274:	0f 8e 7b 08 00 00    	jle    401af5 <main+0x955>
/usr/include/stdlib.h:376

# ifdef __USE_ISOC99
__extension__ __extern_inline long long int
__NTH (atoll (const char *__nptr))
{
  return strtoll (__nptr, (char **) NULL, 10);
  40127a:	49 8b 7d 00          	mov    0x0(%r13),%rdi
  40127e:	ba 0a 00 00 00       	mov    $0xa,%edx
  401283:	31 f6                	xor    %esi,%esi
  401285:	e8 76 fe ff ff       	call   401100 <strtoll@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1945
            gib_global_biginf_init_chunk_size = atoll(argv[i + 1]);
  40128a:	48 89 05 3f 3e 00 00 	mov    %rax,0x3e3f(%rip)        # 4050d0 <gib_global_biginf_init_chunk_size>
  401291:	41 83 c7 02          	add    $0x2,%r15d
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1937
    for (i = 1; i < argc; ++i)
  401295:	41 83 c6 02          	add    $0x2,%r14d
  401299:	49 83 c5 10          	add    $0x10,%r13
  40129d:	41 39 ef             	cmp    %ebp,%r15d
  4012a0:	0f 84 55 02 00 00    	je     4014fb <main+0x35b>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1939
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
  4012a6:	4d 8b 45 f8          	mov    -0x8(%r13),%r8
  4012aa:	41 80 38 2d          	cmpb   $0x2d,(%r8)
  4012ae:	75 12                	jne    4012c2 <main+0x122>
  4012b0:	41 80 78 01 68       	cmpb   $0x68,0x1(%r8)
  4012b5:	75 0b                	jne    4012c2 <main+0x122>
  4012b7:	41 80 78 02 00       	cmpb   $0x0,0x2(%r8)
  4012bc:	0f 84 83 01 00 00    	je     401445 <main+0x2a5>
  4012c2:	bf 54 36 40 00       	mov    $0x403654,%edi
  4012c7:	b9 07 00 00 00       	mov    $0x7,%ecx
  4012cc:	4c 89 c6             	mov    %r8,%rsi
  4012cf:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  4012d1:	0f 97 c0             	seta   %al
  4012d4:	1c 00                	sbb    $0x0,%al
  4012d6:	84 c0                	test   %al,%al
  4012d8:	0f 84 67 01 00 00    	je     401445 <main+0x2a5>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1943
        else if (strcmp(argv[i], "--biginf-buffer-size") == 0 && i < argc - 1) {
  4012de:	bf 5b 36 40 00       	mov    $0x40365b,%edi
  4012e3:	b9 15 00 00 00       	mov    $0x15,%ecx
  4012e8:	4c 89 c6             	mov    %r8,%rsi
  4012eb:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  4012ed:	0f 97 c0             	seta   %al
  4012f0:	1c 00                	sbb    $0x0,%al
  4012f2:	84 c0                	test   %al,%al
  4012f4:	0f 84 6e ff ff ff    	je     401268 <main+0xc8>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1948
        else if (strcmp(argv[i], "--inf-buffer-size") == 0 && i < argc - 1) {
  4012fa:	bf 70 36 40 00       	mov    $0x403670,%edi
  4012ff:	b9 12 00 00 00       	mov    $0x12,%ecx
  401304:	4c 89 c6             	mov    %r8,%rsi
  401307:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  401309:	0f 97 c0             	seta   %al
  40130c:	1c 00                	sbb    $0x0,%al
  40130e:	84 c0                	test   %al,%al
  401310:	75 2e                	jne    401340 <main+0x1a0>
  401312:	45 39 fc             	cmp    %r15d,%r12d
  401315:	7e 29                	jle    401340 <main+0x1a0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  401317:	44 39 f3             	cmp    %r14d,%ebx
  40131a:	0f 8e c1 07 00 00    	jle    401ae1 <main+0x941>
/usr/include/stdlib.h:376
  401320:	49 8b 7d 00          	mov    0x0(%r13),%rdi
  401324:	ba 0a 00 00 00       	mov    $0xa,%edx
  401329:	31 f6                	xor    %esi,%esi
  40132b:	e8 d0 fd ff ff       	call   401100 <strtoll@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1950
            gib_global_inf_init_chunk_size = atoll(argv[i + 1]);
  401330:	48 89 05 91 3d 00 00 	mov    %rax,0x3d91(%rip)        # 4050c8 <gib_global_inf_init_chunk_size>
  401337:	e9 55 ff ff ff       	jmp    401291 <main+0xf1>
  40133c:	0f 1f 40 00          	nopl   0x0(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1953
        else if ((strcmp(argv[i], "--bench-input") == 0)) {
  401340:	bf 82 36 40 00       	mov    $0x403682,%edi
  401345:	b9 0e 00 00 00       	mov    $0xe,%ecx
  40134a:	4c 89 c6             	mov    %r8,%rsi
  40134d:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  40134f:	0f 97 c0             	seta   %al
  401352:	1c 00                	sbb    $0x0,%al
  401354:	84 c0                	test   %al,%al
  401356:	75 18                	jne    401370 <main+0x1d0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  401358:	44 39 f3             	cmp    %r14d,%ebx
  40135b:	0f 8f 30 ff ff ff    	jg     401291 <main+0xf1>
  401361:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  401366:	be 82 36 40 00       	mov    $0x403682,%esi
  40136b:	e8 70 0d 00 00       	call   4020e0 <check_args.part.0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1958
        else if ((strcmp(argv[i], "--array-input") == 0)) {
  401370:	bf 90 36 40 00       	mov    $0x403690,%edi
  401375:	b9 0e 00 00 00       	mov    $0xe,%ecx
  40137a:	4c 89 c6             	mov    %r8,%rsi
  40137d:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  40137f:	0f 97 c0             	seta   %al
  401382:	1c 00                	sbb    $0x0,%al
  401384:	84 c0                	test   %al,%al
  401386:	75 18                	jne    4013a0 <main+0x200>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  401388:	44 39 f3             	cmp    %r14d,%ebx
  40138b:	0f 8f 00 ff ff ff    	jg     401291 <main+0xf1>
  401391:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  401396:	be 90 36 40 00       	mov    $0x403690,%esi
  40139b:	e8 40 0d 00 00       	call   4020e0 <check_args.part.0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1963
        else if (strcmp(argv[i], "--array-input-length") == 0 && i < argc - 1) {
  4013a0:	bf 9e 36 40 00       	mov    $0x40369e,%edi
  4013a5:	b9 15 00 00 00       	mov    $0x15,%ecx
  4013aa:	4c 89 c6             	mov    %r8,%rsi
  4013ad:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  4013af:	0f 97 c0             	seta   %al
  4013b2:	1c 00                	sbb    $0x0,%al
  4013b4:	84 c0                	test   %al,%al
  4013b6:	75 28                	jne    4013e0 <main+0x240>
  4013b8:	45 39 fc             	cmp    %r15d,%r12d
  4013bb:	7e 4b                	jle    401408 <main+0x268>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  4013bd:	44 39 f3             	cmp    %r14d,%ebx
  4013c0:	0f 8e da 07 00 00    	jle    401ba0 <main+0xa00>
/usr/include/stdlib.h:376
  4013c6:	49 8b 7d 00          	mov    0x0(%r13),%rdi
  4013ca:	ba 0a 00 00 00       	mov    $0xa,%edx
  4013cf:	31 f6                	xor    %esi,%esi
  4013d1:	e8 2a fd ff ff       	call   401100 <strtoll@plt>
  4013d6:	e9 b6 fe ff ff       	jmp    401291 <main+0xf1>
  4013db:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1968
        else if (strcmp(argv[i], "--bench-prog") == 0 && i < argc - 1) {
  4013e0:	bf b3 36 40 00       	mov    $0x4036b3,%edi
  4013e5:	b9 0d 00 00 00       	mov    $0xd,%ecx
  4013ea:	4c 89 c6             	mov    %r8,%rsi
  4013ed:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  4013ef:	0f 97 c0             	seta   %al
  4013f2:	1c 00                	sbb    $0x0,%al
  4013f4:	84 c0                	test   %al,%al
  4013f6:	75 10                	jne    401408 <main+0x268>
  4013f8:	45 39 fc             	cmp    %r15d,%r12d
  4013fb:	0f 8f 8f 00 00 00    	jg     401490 <main+0x2f0>
  401401:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1975
            int len = strlen(argv[i+1]);
            gib_global_bench_prog_param = (char*) gib_alloc((len+1)*sizeof(char));
            strncpy(gib_global_bench_prog_param,argv[i+1],len);
            i++;
        }
        else if ((strcmp(argv[i], "--iterate") == 0)) {
  401408:	bf c0 36 40 00       	mov    $0x4036c0,%edi
  40140d:	b9 0a 00 00 00       	mov    $0xa,%ecx
  401412:	4c 89 c6             	mov    %r8,%rsi
  401415:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  401417:	0f 97 c0             	seta   %al
  40141a:	1c 00                	sbb    $0x0,%al
  40141c:	84 c0                	test   %al,%al
  40141e:	75 36                	jne    401456 <main+0x2b6>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  401420:	44 39 f3             	cmp    %r14d,%ebx
  401423:	0f 8e 3d 07 00 00    	jle    401b66 <main+0x9c6>
/usr/include/stdlib.h:376
  401429:	49 8b 7d 00          	mov    0x0(%r13),%rdi
  40142d:	ba 0a 00 00 00       	mov    $0xa,%edx
  401432:	31 f6                	xor    %esi,%esi
  401434:	e8 c7 fc ff ff       	call   401100 <strtoll@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1977
            check_args(i, argc, argv, "--iterate");
            gib_global_iters_param = atoll(argv[i+1]);
  401439:	48 89 05 98 3c 00 00 	mov    %rax,0x3c98(%rip)        # 4050d8 <gib_global_iters_param>
  401440:	e9 4c fe ff ff       	jmp    401291 <main+0xf1>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1940
            gib_show_usage(argv);
  401445:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40144a:	e8 01 09 00 00       	call   401d50 <gib_show_usage>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1941
            exit(0);
  40144f:	31 ff                	xor    %edi,%edi
  401451:	e8 1a fd ff ff       	call   401170 <exit@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1980
            i++;
        }
        else if ((strcmp(argv[i], "--size-param") == 0)) {
  401456:	be ca 36 40 00       	mov    $0x4036ca,%esi
  40145b:	4c 89 c7             	mov    %r8,%rdi
  40145e:	e8 8d fc ff ff       	call   4010f0 <strcmp@plt>
  401463:	85 c0                	test   %eax,%eax
  401465:	0f 85 53 07 00 00    	jne    401bbe <main+0xa1e>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  40146b:	44 39 f3             	cmp    %r14d,%ebx
  40146e:	0f 8e 3b 07 00 00    	jle    401baf <main+0xa0f>
/usr/include/stdlib.h:376
  401474:	49 8b 7d 00          	mov    0x0(%r13),%rdi
  401478:	ba 0a 00 00 00       	mov    $0xa,%edx
  40147d:	31 f6                	xor    %esi,%esi
  40147f:	e8 7c fc ff ff       	call   401100 <strtoll@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1982
            check_args(i, argc, argv, "--size-param");
            gib_global_size_param = atoll(argv[i+1]);
  401484:	48 89 05 55 3c 00 00 	mov    %rax,0x3c55(%rip)        # 4050e0 <gib_global_size_param>
  40148b:	e9 01 fe ff ff       	jmp    401291 <main+0xf1>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1877
    if (i+1 >= argc) {
  401490:	44 39 f3             	cmp    %r14d,%ebx
  401493:	0f 8e 7e 07 00 00    	jle    401c17 <main+0xa77>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1970
            int len = strlen(argv[i+1]);
  401499:	49 8b 75 00          	mov    0x0(%r13),%rsi
  40149d:	48 89 f7             	mov    %rsi,%rdi
  4014a0:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4014a5:	e8 f6 fb ff ff       	call   4010a0 <strlen@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1971
            gib_global_bench_prog_param = (char*) gib_alloc((len+1)*sizeof(char));
  4014aa:	8d 78 01             	lea    0x1(%rax),%edi
  4014ad:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4014b2:	48 63 ff             	movslq %edi,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4014b5:	e8 76 fc ff ff       	call   401130 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1972
            strncpy(gib_global_bench_prog_param,argv[i+1],len);
  4014ba:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  4014bf:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4014c4:	48 89 c7             	mov    %rax,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1971
            gib_global_bench_prog_param = (char*) gib_alloc((len+1)*sizeof(char));
  4014c7:	48 89 05 6a 3c 00 00 	mov    %rax,0x3c6a(%rip)        # 405138 <gib_global_bench_prog_param>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1972
            strncpy(gib_global_bench_prog_param,argv[i+1],len);
  4014ce:	48 63 d2             	movslq %edx,%rdx
  4014d1:	e8 7a fb ff ff       	call   401050 <strncpy@plt>
  4014d6:	e9 b6 fd ff ff       	jmp    401291 <main+0xf1>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1927
            fprintf(stderr, " [gibbon rts] Failed setrlimit stack size to something reasonable; giving up.\n");
  4014db:	ba 4e 00 00 00       	mov    $0x4e,%edx
  4014e0:	be 01 00 00 00       	mov    $0x1,%esi
  4014e5:	bf 58 35 40 00       	mov    $0x403558,%edi
  4014ea:	48 8b 0d 0f 3c 00 00 	mov    0x3c0f(%rip),%rcx        # 405100 <stderr@GLIBC_2.2.5>
  4014f1:	e8 8a fc ff ff       	call   401180 <fwrite@plt>
  4014f6:	e9 3b fd ff ff       	jmp    401236 <main+0x96>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1996
        }
    }

    // Initialize gib_global_bench_prog_param to an empty string in case
    // the runtime argument --bench-prog isn't passed.
    if (gib_global_bench_prog_param == NULL) {
  4014fb:	48 83 3d 35 3c 00 00 	cmpq   $0x0,0x3c35(%rip)        # 405138 <gib_global_bench_prog_param>
  401502:	00 
  401503:	0f 84 a7 05 00 00    	je     401ab0 <main+0x910>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401509:	bf f0 00 00 00       	mov    $0xf0,%edi
  40150e:	e8 1d fc ff ff       	call   401130 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1565
    stats->minor_collections = 0;
  401513:	66 0f ef c0          	pxor   %xmm0,%xmm0
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1581
    stats->gc_elapsed_time = 0;
  401517:	66 0f ef c9          	pxor   %xmm1,%xmm1
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  40151b:	bf 20 00 00 00       	mov    $0x20,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1587
    stats->gc_zct_mgmt_time = 0;
  401520:	48 c7 80 b0 00 00 00 	movq   $0x0,0xb0(%rax)
  401527:	00 00 00 00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1594
    stats->rootset_size = 0;
  40152b:	48 c7 80 e8 00 00 00 	movq   $0x0,0xe8(%rax)
  401532:	00 00 00 00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1565
    stats->minor_collections = 0;
  401536:	0f 11 00             	movups %xmm0,(%rax)
  401539:	0f 11 40 10          	movups %xmm0,0x10(%rax)
  40153d:	0f 11 40 20          	movups %xmm0,0x20(%rax)
  401541:	0f 11 40 30          	movups %xmm0,0x30(%rax)
  401545:	0f 11 40 40          	movups %xmm0,0x40(%rax)
  401549:	0f 11 40 50          	movups %xmm0,0x50(%rax)
  40154d:	0f 11 40 60          	movups %xmm0,0x60(%rax)
  401551:	0f 11 40 70          	movups %xmm0,0x70(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1581
    stats->gc_elapsed_time = 0;
  401555:	0f 11 88 80 00 00 00 	movups %xmm1,0x80(%rax)
  40155c:	0f 11 88 90 00 00 00 	movups %xmm1,0x90(%rax)
  401563:	0f 11 88 a0 00 00 00 	movups %xmm1,0xa0(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1588
    stats->fwd_env_size = 0;
  40156a:	0f 11 80 b8 00 00 00 	movups %xmm0,0xb8(%rax)
  401571:	0f 11 80 c8 00 00 00 	movups %xmm0,0xc8(%rax)
  401578:	0f 11 80 d8 00 00 00 	movups %xmm0,0xd8(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1338
    gib_global_gc_stats = (GibGcStats *) gib_alloc(sizeof(GibGcStats));
  40157f:	48 89 05 8a 3b 00 00 	mov    %rax,0x3b8a(%rip)        # 405110 <gib_global_gc_stats>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401586:	e8 a5 fb ff ff       	call   401130 <malloc@plt>
  40158b:	bf 00 00 40 00       	mov    $0x400000,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1420
    nursery->heap_size = nsize;
  401590:	48 c7 00 00 00 40 00 	movq   $0x400000,(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401597:	48 89 c3             	mov    %rax,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1343
    gib_global_nurseries = (GibNursery *) gib_alloc(gib_global_num_threads *
  40159a:	48 89 05 8f 3b 00 00 	mov    %rax,0x3b8f(%rip)        # 405130 <gib_global_nurseries>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4015a1:	e8 8a fb ff ff       	call   401130 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1421
    nursery->heap_start = (char *) gib_alloc(nsize);
  4015a6:	48 89 43 08          	mov    %rax,0x8(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1422
    if (nursery->heap_start == NULL) {
  4015aa:	48 85 c0             	test   %rax,%rax
  4015ad:	0f 84 93 05 00 00    	je     401b46 <main+0x9a6>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1427
    nursery->heap_end = nursery->heap_start + nsize;
  4015b3:	48 05 00 00 40 00    	add    $0x400000,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4015b9:	bf 18 00 00 00       	mov    $0x18,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1427
    nursery->heap_end = nursery->heap_start + nsize;
  4015be:	48 89 43 10          	mov    %rax,0x10(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1428
    nursery->alloc = nursery->heap_end;
  4015c2:	48 89 43 18          	mov    %rax,0x18(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4015c6:	e8 65 fb ff ff       	call   401130 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1454
    oldgen->old_zct = (void *) NULL;
  4015cb:	66 0f ef c0          	pxor   %xmm0,%xmm0
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4015cf:	bf 18 00 00 00       	mov    $0x18,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1454
    oldgen->old_zct = (void *) NULL;
  4015d4:	0f 11 40 08          	movups %xmm0,0x8(%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4015d8:	48 89 c5             	mov    %rax,%rbp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1350
    gib_global_oldgen = (GibOldgen *) gib_alloc(sizeof(GibOldgen));
  4015db:	48 89 05 36 3b 00 00 	mov    %rax,0x3b36(%rip)        # 405118 <gib_global_oldgen>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4015e2:	e8 49 fb ff ff       	call   401130 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1457
    oldgen->rem_set = (GibRememberedSet *) gib_alloc(sizeof(GibRememberedSet));
  4015e7:	48 89 45 00          	mov    %rax,0x0(%rbp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4015eb:	48 89 c3             	mov    %rax,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1458
    if (oldgen->rem_set == NULL) {
  4015ee:	48 85 c0             	test   %rax,%rax
  4015f1:	0f 84 7e 05 00 00    	je     401b75 <main+0x9d5>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  4015f7:	bf 00 00 00 06       	mov    $0x6000000,%edi
  4015fc:	e8 2f fb ff ff       	call   401130 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1485
    stack->start = (char *) gib_alloc(stack_size);
  401601:	48 89 03             	mov    %rax,(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1486
    if (stack->start == NULL) {
  401604:	48 85 c0             	test   %rax,%rax
  401607:	0f 84 e3 04 00 00    	je     401af0 <main+0x950>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1491
    stack->end = stack->start + stack_size;
  40160d:	48 8d 90 00 00 00 06 	lea    0x6000000(%rax),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1492
    stack->alloc = stack->start;
  401614:	48 89 43 10          	mov    %rax,0x10(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401618:	bf 18 00 00 00       	mov    $0x18,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1491
    stack->end = stack->start + stack_size;
  40161d:	48 89 53 08          	mov    %rdx,0x8(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401621:	e8 0a fb ff ff       	call   401130 <malloc@plt>
  401626:	bf 18 00 00 00       	mov    $0x18,%edi
  40162b:	49 89 c4             	mov    %rax,%r12
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1355
    gib_global_read_shadowstacks =
  40162e:	48 89 05 f3 3a 00 00 	mov    %rax,0x3af3(%rip)        # 405128 <gib_global_read_shadowstacks>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401635:	e8 f6 fa ff ff       	call   401130 <malloc@plt>
  40163a:	bf 00 00 00 06       	mov    $0x6000000,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1358
    gib_global_write_shadowstacks =
  40163f:	48 89 05 da 3a 00 00 	mov    %rax,0x3ada(%rip)        # 405120 <gib_global_write_shadowstacks>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401646:	48 89 c3             	mov    %rax,%rbx
  401649:	e8 e2 fa ff ff       	call   401130 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1485
    stack->start = (char *) gib_alloc(stack_size);
  40164e:	49 89 04 24          	mov    %rax,(%r12)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1486
    if (stack->start == NULL) {
  401652:	48 85 c0             	test   %rax,%rax
  401655:	0f 84 95 04 00 00    	je     401af0 <main+0x950>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1491
    stack->end = stack->start + stack_size;
  40165b:	48 8d 90 00 00 00 06 	lea    0x6000000(%rax),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1492
    stack->alloc = stack->start;
  401662:	49 89 44 24 10       	mov    %rax,0x10(%r12)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401667:	bf 00 00 00 06       	mov    $0x6000000,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1491
    stack->end = stack->start + stack_size;
  40166c:	49 89 54 24 08       	mov    %rdx,0x8(%r12)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401671:	e8 ba fa ff ff       	call   401130 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1485
    stack->start = (char *) gib_alloc(stack_size);
  401676:	48 89 03             	mov    %rax,(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1486
    if (stack->start == NULL) {
  401679:	48 85 c0             	test   %rax,%rax
  40167c:	0f 84 6e 04 00 00    	je     401af0 <main+0x950>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1491
    stack->end = stack->start + stack_size;
  401682:	48 8d 90 00 00 00 06 	lea    0x6000000(%rax),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1492
    stack->alloc = stack->start;
  401689:	48 89 43 10          	mov    %rax,0x10(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:2014

#ifndef _GIBBON_POINTER
    // Initialize the nursery and shadow stack.
    gib_storage_initialize();
    GibOldgen *oldgen = DEFAULT_GENERATION;
    gib_init_zcts(oldgen);
  40168d:	48 89 ef             	mov    %rbp,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1491
    stack->end = stack->start + stack_size;
  401690:	48 89 53 08          	mov    %rdx,0x8(%rbx)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:2014
    gib_init_zcts(oldgen);
  401694:	e8 c7 f9 ff ff       	call   401060 <gib_init_zcts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:2017

    // Minimal test to see if FFI is set up correctly.
    gib_check_rust_struct_sizes();
  401699:	e8 a2 0a 00 00       	call   402140 <gib_check_rust_struct_sizes>
info_table_initialize():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:70
    int error = gib_info_table_initialize(7);
  40169e:	bf 07 00 00 00       	mov    $0x7,%edi
  4016a3:	e8 38 fa ff ff       	call   4010e0 <gib_info_table_initialize@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:72
    if (error < 0) {
  4016a8:	85 c0                	test   %eax,%eax
  4016aa:	0f 88 d1 04 00 00    	js     401b81 <main+0x9e1>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:79
    gib_info_table_finalize();
  4016b0:	e8 ab fa ff ff       	call   401160 <gib_info_table_finalize@plt>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:78
    return gib_global_size_param;
  4016b5:	4c 8b 3d 24 3a 00 00 	mov    0x3a24(%rip),%r15        # 4050e0 <gib_global_size_param>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:93
    if (fltIf_612_633) {
  4016bc:	41 be 00 00 00 00    	mov    $0x0,%r14d
  4016c2:	4d 85 ff             	test   %r15,%r15
  4016c5:	4d 0f 49 f7          	cmovns %r15,%r14
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:155
    
    GibInt n_6_549_620 = gib_get_size_param();
    GibInt n_16_573_596_621 = gib_get_size_param();
    GibInt n__19_575_598_623 =  maxInt(n_16_573_596_621, 0);
    GibInt tmp_7 = sizeof(GibInt);
    GibVector *vec_20_576_599_624 = gib_vector_alloc(n__19_575_598_623, tmp_7);
  4016c9:	4c 89 f7             	mov    %r14,%rdi
  4016cc:	e8 6f 09 00 00       	call   402040 <gib_vector_alloc.constprop.0>
  4016d1:	49 89 c4             	mov    %rax,%r12
generate_loop_398_533():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:105
    if (fltIf_613_638) {
  4016d4:	4d 85 ff             	test   %r15,%r15
  4016d7:	7e 13                	jle    4016ec <main+0x54c>
  4016d9:	4c 89 f9             	mov    %r15,%rcx
  4016dc:	4c 89 f2             	mov    %r14,%rdx
  4016df:	31 f6                	xor    %esi,%esi
  4016e1:	48 89 c7             	mov    %rax,%rdi
  4016e4:	e8 27 07 00 00       	call   401e10 <generate_loop_398_533.part.0>
  4016e9:	49 89 c4             	mov    %rax,%r12
main():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:159
    GibVector *vec1_21_577_600_625 =
               generate_loop_398_533(vec_20_576_599_624, 0, n__19_575_598_623, n_6_549_620);
    GibVector *timed_671;
    GibVector *times_5 = gib_vector_alloc(gib_get_iters_param(),
  4016ec:	48 8b 3d e5 39 00 00 	mov    0x39e5(%rip),%rdi        # 4050d8 <gib_global_iters_param>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:164
                                          sizeof(double));
    struct timespec begin_timed_671;
    struct timespec end_timed_671;
    
    for (long long iters_timed_671 = 0; iters_timed_671 < gib_get_iters_param();
  4016f3:	45 31 f6             	xor    %r14d,%r14d
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:159
    GibVector *times_5 = gib_vector_alloc(gib_get_iters_param(),
  4016f6:	e8 45 09 00 00       	call   402040 <gib_vector_alloc.constprop.0>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:164
    for (long long iters_timed_671 = 0; iters_timed_671 < gib_get_iters_param();
  4016fb:	48 83 3d d5 39 00 00 	cmpq   $0x0,0x39d5(%rip)        # 4050d8 <gib_global_iters_param>
  401702:	00 
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:159
    GibVector *times_5 = gib_vector_alloc(gib_get_iters_param(),
  401703:	48 89 c5             	mov    %rax,%rbp
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:164
    for (long long iters_timed_671 = 0; iters_timed_671 < gib_get_iters_param();
  401706:	0f 8e b8 01 00 00    	jle    4018c4 <main+0x724>
  40170c:	0f 1f 40 00          	nopl   0x0(%rax)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:170
         iters_timed_671++) {
        if (iters_timed_671 != gib_get_iters_param() - 1) {
            gib_list_bumpalloc_save_state();
            gib_ptr_bumpalloc_save_state();
        }
        clock_gettime(CLOCK_MONOTONIC_RAW, &begin_timed_671);
  401710:	48 8d 74 24 40       	lea    0x40(%rsp),%rsi
  401715:	bf 04 00 00 00       	mov    $0x4,%edi
  40171a:	41 bf 00 00 00 00    	mov    $0x0,%r15d
  401720:	e8 6b f9 ff ff       	call   401090 <clock_gettime@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:533
    return (vec->upper - vec->lower);
  401725:	49 8b 5c 24 08       	mov    0x8(%r12),%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  40172a:	bf 20 00 00 00       	mov    $0x20,%edi
  40172f:	49 2b 1c 24          	sub    (%r12),%rbx
  401733:	4c 0f 49 fb          	cmovns %rbx,%r15
  401737:	e8 f4 f9 ff ff       	call   401130 <malloc@plt>
  40173c:	49 89 c5             	mov    %rax,%r13
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:515
    if (vec == NULL) {
  40173f:	48 85 c0             	test   %rax,%rax
  401742:	0f 84 dc 03 00 00    	je     401b24 <main+0x984>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:519
    void *data = (void *) gib_alloc(num * elt_size);
  401748:	4a 8d 3c fd 00 00 00 	lea    0x0(,%r15,8),%rdi
  40174f:	00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401750:	e8 db f9 ff ff       	call   401130 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:520
    if (data == NULL) {
  401755:	48 85 c0             	test   %rax,%rax
  401758:	0f 84 a6 03 00 00    	je     401b04 <main+0x964>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:524
    vec->lower = 0;
  40175e:	49 c7 45 00 00 00 00 	movq   $0x0,0x0(%r13)
  401765:	00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:525
    vec->upper = num;
  401766:	4d 89 7d 08          	mov    %r15,0x8(%r13)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:526
    vec->elt_size = elt_size;
  40176a:	49 c7 45 10 08 00 00 	movq   $0x8,0x10(%r13)
  401771:	00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:527
    vec->data = data;
  401772:	49 89 45 18          	mov    %rax,0x18(%r13)
generate_loop_398_536():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:125
    if (fltIf_616_653) {
  401776:	48 85 db             	test   %rbx,%rbx
  401779:	0f 8e c1 00 00 00    	jle    401840 <main+0x6a0>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  40177f:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
  401784:	49 0f af 14 24       	imul   (%r12),%rdx
generate_loop_398_536():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:132
        GibInt fltAppE_618_656 = *tmp_0;
  401789:	49 8b 4c 24 18       	mov    0x18(%r12),%rcx
add1():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:85
    GibInt tailprim_667 = x_10_552_630 + 1;
  40178e:	48 8b 14 11          	mov    (%rcx,%rdx,1),%rdx
  401792:	48 83 c2 01          	add    $0x1,%rdx
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401796:	48 89 10             	mov    %rdx,(%rax)
generate_loop_398_536():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:125
    if (fltIf_616_653) {
  401799:	48 83 fb 01          	cmp    $0x1,%rbx
  40179d:	0f 84 9d 00 00 00    	je     401840 <main+0x6a0>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4017a3:	49 8b 14 24          	mov    (%r12),%rdx
generate_loop_398_536():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:132
        GibInt fltAppE_618_656 = *tmp_0;
  4017a7:	49 8b 4c 24 18       	mov    0x18(%r12),%rcx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4017ac:	48 83 c2 01          	add    $0x1,%rdx
  4017b0:	49 0f af 54 24 10    	imul   0x10(%r12),%rdx
add1():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:85
    GibInt tailprim_667 = x_10_552_630 + 1;
  4017b6:	48 8b 14 11          	mov    (%rcx,%rdx,1),%rdx
  4017ba:	48 83 c2 01          	add    $0x1,%rdx
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4017be:	48 89 50 08          	mov    %rdx,0x8(%rax)
generate_loop_398_536():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:125
    if (fltIf_616_653) {
  4017c2:	48 83 fb 02          	cmp    $0x2,%rbx
  4017c6:	74 78                	je     401840 <main+0x6a0>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4017c8:	49 8b 14 24          	mov    (%r12),%rdx
generate_loop_398_536():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:132
        GibInt fltAppE_618_656 = *tmp_0;
  4017cc:	49 8b 4c 24 18       	mov    0x18(%r12),%rcx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4017d1:	48 83 c2 02          	add    $0x2,%rdx
  4017d5:	49 0f af 54 24 10    	imul   0x10(%r12),%rdx
add1():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:85
    GibInt tailprim_667 = x_10_552_630 + 1;
  4017db:	48 8b 14 11          	mov    (%rcx,%rdx,1),%rdx
  4017df:	48 83 c2 01          	add    $0x1,%rdx
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4017e3:	48 89 50 10          	mov    %rdx,0x10(%rax)
generate_loop_398_536():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:125
    if (fltIf_616_653) {
  4017e7:	48 83 fb 03          	cmp    $0x3,%rbx
  4017eb:	74 53                	je     401840 <main+0x6a0>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4017ed:	49 8b 14 24          	mov    (%r12),%rdx
generate_loop_398_536():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:132
        GibInt fltAppE_618_656 = *tmp_0;
  4017f1:	49 8b 4c 24 18       	mov    0x18(%r12),%rcx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4017f6:	48 83 c2 03          	add    $0x3,%rdx
  4017fa:	49 0f af 54 24 10    	imul   0x10(%r12),%rdx
add1():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:85
    GibInt tailprim_667 = x_10_552_630 + 1;
  401800:	48 8b 14 11          	mov    (%rcx,%rdx,1),%rdx
  401804:	48 83 c2 01          	add    $0x1,%rdx
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401808:	48 89 50 18          	mov    %rdx,0x18(%rax)
generate_loop_398_536():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:125
    if (fltIf_616_653) {
  40180c:	48 83 fb 04          	cmp    $0x4,%rbx
  401810:	74 2e                	je     401840 <main+0x6a0>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401812:	49 8b 14 24          	mov    (%r12),%rdx
generate_loop_398_536():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:132
        GibInt fltAppE_618_656 = *tmp_0;
  401816:	49 8b 4c 24 18       	mov    0x18(%r12),%rcx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  40181b:	48 83 c2 04          	add    $0x4,%rdx
  40181f:	49 0f af 54 24 10    	imul   0x10(%r12),%rdx
add1():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:85
    GibInt tailprim_667 = x_10_552_630 + 1;
  401825:	48 8b 14 11          	mov    (%rcx,%rdx,1),%rdx
  401829:	48 83 c2 01          	add    $0x1,%rdx
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  40182d:	48 89 50 20          	mov    %rdx,0x20(%rax)
generate_loop_398_536():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:125
    if (fltIf_616_653) {
  401831:	48 83 fb 05          	cmp    $0x5,%rbx
  401835:	0f 85 8e 02 00 00    	jne    401ac9 <main+0x929>
  40183b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
main():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:181
                                                         tmp_1);
        GibVector *vec1_21_590_648_664 =
                   generate_loop_398_536(vec_20_589_647_663, 0, n__19_588_646_662, vec1_21_577_600_625);
        
        timed_671 = vec1_21_590_648_664;
        clock_gettime(CLOCK_MONOTONIC_RAW, &end_timed_671);
  401840:	48 8d 74 24 30       	lea    0x30(%rsp),%rsi
  401845:	bf 04 00 00 00       	mov    $0x4,%edi
  40184a:	e8 41 f8 ff ff       	call   401090 <clock_gettime@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1828
            + ((double)(t1->tv_nsec - t0->tv_nsec) / 1000000000.0);
  40184f:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  401854:	66 0f ef c0          	pxor   %xmm0,%xmm0
  401858:	48 2b 44 24 48       	sub    0x48(%rsp),%rax
  40185d:	f2 48 0f 2a c0       	cvtsi2sd %rax,%xmm0
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1827
    return (double)(t1->tv_sec - t0->tv_sec)
  401862:	66 0f ef c9          	pxor   %xmm1,%xmm1
  401866:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  40186b:	48 2b 44 24 40       	sub    0x40(%rsp),%rax
  401870:	f2 48 0f 2a c8       	cvtsi2sd %rax,%xmm1
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:189
            gib_ptr_bumpalloc_restore_state();
        }
        
        double itertime_2 = gib_difftimespecs(&begin_timed_671, &end_timed_671);
        
        printf("itertime: %lf\n", itertime_2);
  401875:	bf f7 36 40 00       	mov    $0x4036f7,%edi
  40187a:	b8 01 00 00 00       	mov    $0x1,%eax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1828
            + ((double)(t1->tv_nsec - t0->tv_nsec) / 1000000000.0);
  40187f:	f2 0f 5e 05 01 1f 00 	divsd  0x1f01(%rip),%xmm0        # 403788 <__PRETTY_FUNCTION__.3+0x28>
  401886:	00 
  401887:	f2 0f 58 c1          	addsd  %xmm1,%xmm0
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:187
        double itertime_2 = gib_difftimespecs(&begin_timed_671, &end_timed_671);
  40188b:	f2 0f 11 44 24 28    	movsd  %xmm0,0x28(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:189
        printf("itertime: %lf\n", itertime_2);
  401891:	e8 1a f8 ff ff       	call   4010b0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401896:	48 8b 7d 00          	mov    0x0(%rbp),%rdi
  40189a:	48 8b 55 10          	mov    0x10(%rbp),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  40189e:	48 8d 74 24 28       	lea    0x28(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4018a3:	4c 01 f7             	add    %r14,%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:165
         iters_timed_671++) {
  4018a6:	49 83 c6 01          	add    $0x1,%r14
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  4018aa:	48 0f af fa          	imul   %rdx,%rdi
  4018ae:	48 03 7d 18          	add    0x18(%rbp),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  4018b2:	e8 69 f8 ff ff       	call   401120 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:164
    for (long long iters_timed_671 = 0; iters_timed_671 < gib_get_iters_param();
  4018b7:	4c 3b 35 1a 38 00 00 	cmp    0x381a(%rip),%r14        # 4050d8 <gib_global_iters_param>
  4018be:	0f 8c 4c fe ff ff    	jl     401710 <main+0x570>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4018c4:	48 8b 55 10          	mov    0x10(%rbp),%rdx
  4018c8:	48 8b 7d 00          	mov    0x0(%rbp),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:599
    qsort(start, gib_vector_length(vec), vec->elt_size, compar);
  4018cc:	b9 30 1d 40 00       	mov    $0x401d30,%ecx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:533
    return (vec->upper - vec->lower);
  4018d1:	48 8b 75 08          	mov    0x8(%rbp),%rsi
  4018d5:	48 29 fe             	sub    %rdi,%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4018d8:	48 0f af fa          	imul   %rdx,%rdi
  4018dc:	48 03 7d 18          	add    0x18(%rbp),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:599
    qsort(start, gib_vector_length(vec), vec->elt_size, compar);
  4018e0:	e8 9b f7 ff ff       	call   401080 <qsort@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:194
        gib_vector_inplace_update(times_5, iters_timed_671, &itertime_2);
    }
    gib_vector_inplace_sort(times_5, gib_compare_doubles);
    
    double *tmp_6 = (double *) gib_vector_nth(times_5, gib_get_iters_param() /
  4018e5:	48 8b 05 ec 37 00 00 	mov    0x37ec(%rip),%rax        # 4050d8 <gib_global_iters_param>
  4018ec:	41 b8 02 00 00 00    	mov    $0x2,%r8d
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  4018f2:	48 8b 7d 00          	mov    0x0(%rbp),%rdi
  4018f6:	48 8b 75 10          	mov    0x10(%rbp),%rsi
  4018fa:	48 8b 4d 18          	mov    0x18(%rbp),%rcx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:194
  4018fe:	48 99                	cqto
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:693
    double acc = 0;
  401900:	4c 8b 25 79 1e 00 00 	mov    0x1e79(%rip),%r12        # 403780 <__PRETTY_FUNCTION__.3+0x20>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:194
  401907:	49 f7 f8             	idiv   %r8
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:533
    return (vec->upper - vec->lower);
  40190a:	48 8b 55 08          	mov    0x8(%rbp),%rdx
  40190e:	48 29 fa             	sub    %rdi,%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401911:	48 01 f8             	add    %rdi,%rax
  401914:	48 0f af c6          	imul   %rsi,%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:196
                                              2);
    double selftimed_4 = *tmp_6;
  401918:	4c 8b 34 01          	mov    (%rcx,%rax,1),%r14
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:694
    for(int i = 0; i < gib_vector_length(times); i++) {
  40191c:	48 85 d2             	test   %rdx,%rdx
  40191f:	7e 2d                	jle    40194e <main+0x7ae>
  401921:	48 0f af fe          	imul   %rsi,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:693
    double acc = 0;
  401925:	4c 8b 25 54 1e 00 00 	mov    0x1e54(%rip),%r12        # 403780 <__PRETTY_FUNCTION__.3+0x20>
  40192c:	48 8d 04 39          	lea    (%rcx,%rdi,1),%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:696
        acc += *d;
  401930:	66 49 0f 6e d4       	movq   %r12,%xmm2
  401935:	f2 0f 58 10          	addsd  (%rax),%xmm2
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:694
    for(int i = 0; i < gib_vector_length(times); i++) {
  401939:	83 44 24 04 01       	addl   $0x1,0x4(%rsp)
  40193e:	48 01 f0             	add    %rsi,%rax
  401941:	8b 5c 24 04          	mov    0x4(%rsp),%ebx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:696
        acc += *d;
  401945:	66 49 0f 7e d4       	movq   %xmm2,%r12
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:694
    for(int i = 0; i < gib_vector_length(times); i++) {
  40194a:	39 d3                	cmp    %edx,%ebx
  40194c:	75 e2                	jne    401930 <main+0x790>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:674
    printf("ITER TIMES: [");
  40194e:	bf 06 37 40 00       	mov    $0x403706,%edi
  401953:	31 c0                	xor    %eax,%eax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:677
    for(GibInt i = 0; i < n; i++) {
  401955:	31 db                	xor    %ebx,%ebx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:674
    printf("ITER TIMES: [");
  401957:	e8 54 f7 ff ff       	call   4010b0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:533
    return (vec->upper - vec->lower);
  40195c:	4c 8b 6d 08          	mov    0x8(%rbp),%r13
  401960:	4c 2b 6d 00          	sub    0x0(%rbp),%r13
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:679
        if (i == (n-1)) {
  401964:	4d 8d 7d ff          	lea    -0x1(%r13),%r15
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:677
    for(GibInt i = 0; i < n; i++) {
  401968:	4d 85 ed             	test   %r13,%r13
  40196b:	7e 39                	jle    4019a6 <main+0x806>
  40196d:	0f 1f 00             	nopl   (%rax)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401970:	48 8b 45 00          	mov    0x0(%rbp),%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:680
            printf("%f",*d);
  401974:	48 8b 55 18          	mov    0x18(%rbp),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401978:	48 01 d8             	add    %rbx,%rax
  40197b:	48 0f af 45 10       	imul   0x10(%rbp),%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:680
            printf("%f",*d);
  401980:	f2 0f 10 04 02       	movsd  (%rdx,%rax,1),%xmm0
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:679
        if (i == (n-1)) {
  401985:	49 39 df             	cmp    %rbx,%r15
  401988:	0f 84 01 01 00 00    	je     401a8f <main+0x8ef>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:683
            printf("%f, ",*d);
  40198e:	bf 5b 37 40 00       	mov    $0x40375b,%edi
  401993:	b8 01 00 00 00       	mov    $0x1,%eax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:677
    for(GibInt i = 0; i < n; i++) {
  401998:	48 83 c3 01          	add    $0x1,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:683
            printf("%f, ",*d);
  40199c:	e8 0f f7 ff ff       	call   4010b0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:677
    for(GibInt i = 0; i < n; i++) {
  4019a1:	49 39 dd             	cmp    %rbx,%r13
  4019a4:	75 ca                	jne    401970 <main+0x7d0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:686
    printf("]\n");
  4019a6:	bf 14 37 40 00       	mov    $0x403714,%edi
  4019ab:	e8 c0 f6 ff ff       	call   401070 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:242
void gib_free(void *ptr) { free(ptr); }
  4019b0:	48 8b 7d 18          	mov    0x18(%rbp),%rdi
  4019b4:	e8 77 f6 ff ff       	call   401030 <free@plt>
  4019b9:	48 89 ef             	mov    %rbp,%rdi
  4019bc:	e8 6f f6 ff ff       	call   401030 <free@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:201
    double batchtime_3 = gib_sum_timing_array(times_5);
    
    gib_print_timing_array(times_5);
    gib_vector_free(times_5);
    printf("ITERS: %ld\n", gib_get_iters_param());
  4019c1:	48 8b 35 10 37 00 00 	mov    0x3710(%rip),%rsi        # 4050d8 <gib_global_iters_param>
  4019c8:	bf 16 37 40 00       	mov    $0x403716,%edi
  4019cd:	31 c0                	xor    %eax,%eax
  4019cf:	e8 dc f6 ff ff       	call   4010b0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:202
    printf("SIZE: %ld\n", gib_get_size_param());
  4019d4:	48 8b 35 05 37 00 00 	mov    0x3705(%rip),%rsi        # 4050e0 <gib_global_size_param>
  4019db:	bf 22 37 40 00       	mov    $0x403722,%edi
  4019e0:	31 c0                	xor    %eax,%eax
  4019e2:	e8 c9 f6 ff ff       	call   4010b0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:203
    printf("BATCHTIME: %e\n", batchtime_3);
  4019e7:	66 49 0f 6e c4       	movq   %r12,%xmm0
  4019ec:	bf 2d 37 40 00       	mov    $0x40372d,%edi
  4019f1:	b8 01 00 00 00       	mov    $0x1,%eax
  4019f6:	e8 b5 f6 ff ff       	call   4010b0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:204
    printf("SELFTIMED: %e\n", selftimed_4);
  4019fb:	66 49 0f 6e c6       	movq   %r14,%xmm0
  401a00:	bf 3c 37 40 00       	mov    $0x40373c,%edi
  401a05:	b8 01 00 00 00       	mov    $0x1,%eax
  401a0a:	e8 a1 f6 ff ff       	call   4010b0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:205
    printf("'#(");
  401a0f:	bf 4b 37 40 00       	mov    $0x40374b,%edi
  401a14:	31 c0                	xor    %eax,%eax
  401a16:	e8 95 f6 ff ff       	call   4010b0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:206
    printf("<vector>");
  401a1b:	bf 4f 37 40 00       	mov    $0x40374f,%edi
  401a20:	31 c0                	xor    %eax,%eax
  401a22:	e8 89 f6 ff ff       	call   4010b0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:207
    printf(" ");
  401a27:	bf 20 00 00 00       	mov    $0x20,%edi
  401a2c:	e8 0f f6 ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:208
    printf("<vector>");
  401a31:	bf 4f 37 40 00       	mov    $0x40374f,%edi
  401a36:	31 c0                	xor    %eax,%eax
  401a38:	e8 73 f6 ff ff       	call   4010b0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:209
    printf(")");
  401a3d:	bf 29 00 00 00       	mov    $0x29,%edi
  401a42:	e8 f9 f5 ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:210
    printf("\n");
  401a47:	bf 0a 00 00 00       	mov    $0xa,%edi
  401a4c:	e8 ef f5 ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:242
  401a51:	48 8b 3d e0 36 00 00 	mov    0x36e0(%rip),%rdi        # 405138 <gib_global_bench_prog_param>
  401a58:	e8 d3 f5 ff ff       	call   401030 <free@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:2042
    GibShadowstack *rstack = DEFAULT_READ_SHADOWSTACK;
    GibShadowstack *wstack = DEFAULT_WRITE_SHADOWSTACK;
    GibOldgen *oldgen = DEFAULT_GENERATION;

    // Free all objects initialized by the Rust RTS.
    gib_gc_cleanup(rstack, wstack, nursery, oldgen);
  401a5d:	48 8b 0d b4 36 00 00 	mov    0x36b4(%rip),%rcx        # 405118 <gib_global_oldgen>
  401a64:	48 8b 15 c5 36 00 00 	mov    0x36c5(%rip),%rdx        # 405130 <gib_global_nurseries>
  401a6b:	48 8b 35 ae 36 00 00 	mov    0x36ae(%rip),%rsi        # 405120 <gib_global_write_shadowstacks>
  401a72:	48 8b 3d af 36 00 00 	mov    0x36af(%rip),%rdi        # 405128 <gib_global_read_shadowstacks>
  401a79:	e8 42 f6 ff ff       	call   4010c0 <gib_gc_cleanup@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:215
    
    int exit_9 = gib_exit();
    
    return exit_9;
  401a7e:	48 83 c4 58          	add    $0x58,%rsp
  401a82:	31 c0                	xor    %eax,%eax
  401a84:	5b                   	pop    %rbx
  401a85:	5d                   	pop    %rbp
  401a86:	41 5c                	pop    %r12
  401a88:	41 5d                	pop    %r13
  401a8a:	41 5e                	pop    %r14
  401a8c:	41 5f                	pop    %r15
  401a8e:	c3                   	ret
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:680
            printf("%f",*d);
  401a8f:	bf 58 37 40 00       	mov    $0x403758,%edi
  401a94:	b8 01 00 00 00       	mov    $0x1,%eax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:677
    for(GibInt i = 0; i < n; i++) {
  401a99:	49 8d 5f 01          	lea    0x1(%r15),%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:680
            printf("%f",*d);
  401a9d:	e8 0e f6 ff ff       	call   4010b0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:677
    for(GibInt i = 0; i < n; i++) {
  401aa2:	4c 39 eb             	cmp    %r13,%rbx
  401aa5:	0f 85 c5 fe ff ff    	jne    401970 <main+0x7d0>
  401aab:	e9 f6 fe ff ff       	jmp    4019a6 <main+0x806>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  401ab0:	bf 01 00 00 00       	mov    $0x1,%edi
  401ab5:	e8 76 f6 ff ff       	call   401130 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1997
        gib_global_bench_prog_param = (char*) gib_alloc(1*sizeof(char));
  401aba:	48 89 05 77 36 00 00 	mov    %rax,0x3677(%rip)        # 405138 <gib_global_bench_prog_param>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1998
        *gib_global_bench_prog_param = '\n';
  401ac1:	c6 00 0a             	movb   $0xa,(%rax)
  401ac4:	e9 40 fa ff ff       	jmp    401509 <main+0x369>
generate_loop_398_536():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1998
  401ac9:	4c 89 e1             	mov    %r12,%rcx
  401acc:	4c 89 fa             	mov    %r15,%rdx
  401acf:	be 05 00 00 00       	mov    $0x5,%esi
  401ad4:	4c 89 ef             	mov    %r13,%rdi
  401ad7:	e8 34 04 00 00       	call   401f10 <generate_loop_398_536.part.0.isra.0>
  401adc:	e9 5f fd ff ff       	jmp    401840 <main+0x6a0>
  401ae1:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  401ae6:	be 70 36 40 00       	mov    $0x403670,%esi
  401aeb:	e8 f0 05 00 00       	call   4020e0 <check_args.part.0>
  401af0:	e8 1b 06 00 00       	call   402110 <gib_shadowstack_initialize.part.0.constprop.0>
  401af5:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  401afa:	be 5b 36 40 00       	mov    $0x40365b,%esi
  401aff:	e8 dc 05 00 00       	call   4020e0 <check_args.part.0>
main():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:521
        fprintf(stderr, "alloc_vector: gib_alloc failed: %zu", sizeof(num * elt_size));
  401b04:	48 8b 3d f5 35 00 00 	mov    0x35f5(%rip),%rdi        # 405100 <stderr@GLIBC_2.2.5>
  401b0b:	ba 08 00 00 00       	mov    $0x8,%edx
  401b10:	be 18 33 40 00       	mov    $0x403318,%esi
  401b15:	e8 f6 f5 ff ff       	call   401110 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:522
        exit(1);
  401b1a:	bf 01 00 00 00       	mov    $0x1,%edi
  401b1f:	e8 4c f6 ff ff       	call   401170 <exit@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:516
        fprintf(stderr, "alloc_vector: gib_alloc failed: %zu", sizeof(GibVector));
  401b24:	ba 20 00 00 00       	mov    $0x20,%edx
  401b29:	be 18 33 40 00       	mov    $0x403318,%esi
  401b2e:	48 8b 3d cb 35 00 00 	mov    0x35cb(%rip),%rdi        # 405100 <stderr@GLIBC_2.2.5>
  401b35:	31 c0                	xor    %eax,%eax
  401b37:	e8 d4 f5 ff ff       	call   401110 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:517
        exit(1);
  401b3c:	bf 01 00 00 00       	mov    $0x1,%edi
  401b41:	e8 2a f6 ff ff       	call   401170 <exit@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1423
        fprintf(stderr, "gib_nursery_initialize: gib_alloc failed: %zu",
  401b46:	48 8b 3d b3 35 00 00 	mov    0x35b3(%rip),%rdi        # 405100 <stderr@GLIBC_2.2.5>
  401b4d:	ba 00 00 40 00       	mov    $0x400000,%edx
  401b52:	be a8 35 40 00       	mov    $0x4035a8,%esi
  401b57:	e8 b4 f5 ff ff       	call   401110 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1425
        exit(1);
  401b5c:	bf 01 00 00 00       	mov    $0x1,%edi
  401b61:	e8 0a f6 ff ff       	call   401170 <exit@plt>
  401b66:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  401b6b:	be c0 36 40 00       	mov    $0x4036c0,%esi
  401b70:	e8 6b 05 00 00       	call   4020e0 <check_args.part.0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1459
        fprintf(stderr, "gib_oldgen_initialize: gib_alloc failed: %zu",
  401b75:	ba 18 00 00 00       	mov    $0x18,%edx
  401b7a:	be d8 35 40 00       	mov    $0x4035d8,%esi
  401b7f:	eb ad                	jmp    401b2e <main+0x98e>
info_table_initialize():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:73
        fprintf(stderr, "Couldn't initialize info table, errorno=%d", error);
  401b81:	89 c2                	mov    %eax,%edx
  401b83:	be 08 36 40 00       	mov    $0x403608,%esi
  401b88:	48 8b 3d 71 35 00 00 	mov    0x3571(%rip),%rdi        # 405100 <stderr@GLIBC_2.2.5>
  401b8f:	31 c0                	xor    %eax,%eax
  401b91:	e8 7a f5 ff ff       	call   401110 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:74
        exit(1);
  401b96:	bf 01 00 00 00       	mov    $0x1,%edi
  401b9b:	e8 d0 f5 ff ff       	call   401170 <exit@plt>
  401ba0:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  401ba5:	be 9e 36 40 00       	mov    $0x40369e,%esi
  401baa:	e8 31 05 00 00       	call   4020e0 <check_args.part.0>
  401baf:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  401bb4:	be ca 36 40 00       	mov    $0x4036ca,%esi
  401bb9:	e8 22 05 00 00       	call   4020e0 <check_args.part.0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1987
            fprintf(stderr, "Extra arguments left over: ");
  401bbe:	48 8b 0d 3b 35 00 00 	mov    0x353b(%rip),%rcx        # 405100 <stderr@GLIBC_2.2.5>
  401bc5:	ba 1b 00 00 00       	mov    $0x1b,%edx
  401bca:	be 01 00 00 00       	mov    $0x1,%esi
  401bcf:	4d 63 ff             	movslq %r15d,%r15
  401bd2:	bf d7 36 40 00       	mov    $0x4036d7,%edi
  401bd7:	e8 a4 f5 ff ff       	call   401180 <fwrite@plt>
  401bdc:	eb 20                	jmp    401bfe <main+0xa5e>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1988
            for(; i < argc; i++) fprintf(stderr, "%s ", argv[i]);
  401bde:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  401be3:	48 8b 3d 16 35 00 00 	mov    0x3516(%rip),%rdi        # 405100 <stderr@GLIBC_2.2.5>
  401bea:	be f3 36 40 00       	mov    $0x4036f3,%esi
  401bef:	4a 8b 14 f8          	mov    (%rax,%r15,8),%rdx
  401bf3:	31 c0                	xor    %eax,%eax
  401bf5:	49 83 c7 01          	add    $0x1,%r15
  401bf9:	e8 12 f5 ff ff       	call   401110 <fprintf@plt>
  401bfe:	44 39 fb             	cmp    %r15d,%ebx
  401c01:	7f db                	jg     401bde <main+0xa3e>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1989
            gib_show_usage(argv);
  401c03:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  401c08:	e8 43 01 00 00       	call   401d50 <gib_show_usage>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1990
            exit(1);
  401c0d:	bf 01 00 00 00       	mov    $0x1,%edi
  401c12:	e8 59 f5 ff ff       	call   401170 <exit@plt>
  401c17:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  401c1c:	be b3 36 40 00       	mov    $0x4036b3,%esi
  401c21:	e8 ba 04 00 00       	call   4020e0 <check_args.part.0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1910
        fprintf(stderr, " [gibbon rts] failed to getrlimit, code %d\n", code);
  401c26:	89 c2                	mov    %eax,%edx
  401c28:	be f0 34 40 00       	mov    $0x4034f0,%esi
  401c2d:	e9 56 ff ff ff       	jmp    401b88 <main+0x9e8>
main():
  401c32:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  401c39:	00 00 00 
  401c3c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401c40 <_start>:
_start():
  401c40:	f3 0f 1e fa          	endbr64
  401c44:	31 ed                	xor    %ebp,%ebp
  401c46:	49 89 d1             	mov    %rdx,%r9
  401c49:	5e                   	pop    %rsi
  401c4a:	48 89 e2             	mov    %rsp,%rdx
  401c4d:	48 83 e4 f0          	and    $0xfffffffffffffff0,%rsp
  401c51:	50                   	push   %rax
  401c52:	54                   	push   %rsp
  401c53:	45 31 c0             	xor    %r8d,%r8d
  401c56:	31 c9                	xor    %ecx,%ecx
  401c58:	48 c7 c7 a0 11 40 00 	mov    $0x4011a0,%rdi
  401c5f:	ff 15 73 33 00 00    	call   *0x3373(%rip)        # 404fd8 <__libc_start_main@GLIBC_2.34>
  401c65:	f4                   	hlt
  401c66:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  401c6d:	00 00 00 

0000000000401c70 <_dl_relocate_static_pie>:
_dl_relocate_static_pie():
  401c70:	f3 0f 1e fa          	endbr64
  401c74:	c3                   	ret
  401c75:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  401c7c:	00 00 00 
  401c7f:	90                   	nop

0000000000401c80 <deregister_tm_clones>:
deregister_tm_clones():
  401c80:	b8 e8 50 40 00       	mov    $0x4050e8,%eax
  401c85:	48 3d e8 50 40 00    	cmp    $0x4050e8,%rax
  401c8b:	74 13                	je     401ca0 <deregister_tm_clones+0x20>
  401c8d:	b8 00 00 00 00       	mov    $0x0,%eax
  401c92:	48 85 c0             	test   %rax,%rax
  401c95:	74 09                	je     401ca0 <deregister_tm_clones+0x20>
  401c97:	bf e8 50 40 00       	mov    $0x4050e8,%edi
  401c9c:	ff e0                	jmp    *%rax
  401c9e:	66 90                	xchg   %ax,%ax
  401ca0:	c3                   	ret
  401ca1:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  401ca8:	00 00 00 00 
  401cac:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401cb0 <register_tm_clones>:
register_tm_clones():
  401cb0:	be e8 50 40 00       	mov    $0x4050e8,%esi
  401cb5:	48 81 ee e8 50 40 00 	sub    $0x4050e8,%rsi
  401cbc:	48 89 f0             	mov    %rsi,%rax
  401cbf:	48 c1 ee 3f          	shr    $0x3f,%rsi
  401cc3:	48 c1 f8 03          	sar    $0x3,%rax
  401cc7:	48 01 c6             	add    %rax,%rsi
  401cca:	48 d1 fe             	sar    $1,%rsi
  401ccd:	74 11                	je     401ce0 <register_tm_clones+0x30>
  401ccf:	b8 00 00 00 00       	mov    $0x0,%eax
  401cd4:	48 85 c0             	test   %rax,%rax
  401cd7:	74 07                	je     401ce0 <register_tm_clones+0x30>
  401cd9:	bf e8 50 40 00       	mov    $0x4050e8,%edi
  401cde:	ff e0                	jmp    *%rax
  401ce0:	c3                   	ret
  401ce1:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  401ce8:	00 00 00 00 
  401cec:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401cf0 <__do_global_dtors_aux>:
__do_global_dtors_aux():
  401cf0:	80 3d 11 34 00 00 00 	cmpb   $0x0,0x3411(%rip)        # 405108 <completed.0>
  401cf7:	75 17                	jne    401d10 <__do_global_dtors_aux+0x20>
  401cf9:	55                   	push   %rbp
  401cfa:	48 89 e5             	mov    %rsp,%rbp
  401cfd:	e8 7e ff ff ff       	call   401c80 <deregister_tm_clones>
  401d02:	c6 05 ff 33 00 00 01 	movb   $0x1,0x33ff(%rip)        # 405108 <completed.0>
  401d09:	5d                   	pop    %rbp
  401d0a:	c3                   	ret
  401d0b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
  401d10:	c3                   	ret
  401d11:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  401d18:	00 00 00 00 
  401d1c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401d20 <frame_dummy>:
frame_dummy():
  401d20:	eb 8e                	jmp    401cb0 <register_tm_clones>
  401d22:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  401d29:	00 00 00 
  401d2c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401d30 <gib_compare_doubles>:
gib_compare_doubles():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1835
    return (*da > *db) - (*da < *db);
  401d30:	f2 0f 10 07          	movsd  (%rdi),%xmm0
  401d34:	f2 0f 10 0e          	movsd  (%rsi),%xmm1
  401d38:	31 c0                	xor    %eax,%eax
  401d3a:	66 0f 2f c1          	comisd %xmm1,%xmm0
  401d3e:	0f 97 c0             	seta   %al
  401d41:	31 d2                	xor    %edx,%edx
  401d43:	66 0f 2f c8          	comisd %xmm0,%xmm1
  401d47:	0f 97 c2             	seta   %dl
  401d4a:	29 d0                	sub    %edx,%eax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1836
}
  401d4c:	c3                   	ret
  401d4d:	0f 1f 00             	nopl   (%rax)

0000000000401d50 <gib_show_usage>:
gib_show_usage():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1797
{
  401d50:	53                   	push   %rbx
  401d51:	48 89 fb             	mov    %rdi,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1798
    printf("\n");
  401d54:	bf 0a 00 00 00       	mov    $0xa,%edi
  401d59:	e8 e2 f2 ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1799
    printf("This binary was generated by the Gibbon compiler.\n");
  401d5e:	bf 08 30 40 00       	mov    $0x403008,%edi
  401d63:	e8 08 f3 ff ff       	call   401070 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1800
    printf("\n");
  401d68:	bf 0a 00 00 00       	mov    $0xa,%edi
  401d6d:	e8 ce f2 ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1801
    printf("Usage: %s [OPTIONS...]\n", argv[0]);
  401d72:	48 8b 33             	mov    (%rbx),%rsi
  401d75:	bf 33 36 40 00       	mov    $0x403633,%edi
  401d7a:	31 c0                	xor    %eax,%eax
  401d7c:	e8 2f f3 ff ff       	call   4010b0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1803
    printf("\n");
  401d81:	bf 0a 00 00 00       	mov    $0xa,%edi
  401d86:	e8 b5 f2 ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1804
    printf("Options:\n");
  401d8b:	bf 4b 36 40 00       	mov    $0x40364b,%edi
  401d90:	e8 db f2 ff ff       	call   401070 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1805
    printf(" --biginf-buffer-size <bytes>   Set the buffer size (default %" PRId64 ").\n", gib_global_biginf_init_chunk_size);
  401d95:	48 8b 35 34 33 00 00 	mov    0x3334(%rip),%rsi        # 4050d0 <gib_global_biginf_init_chunk_size>
  401d9c:	bf 40 30 40 00       	mov    $0x403040,%edi
  401da1:	31 c0                	xor    %eax,%eax
  401da3:	e8 08 f3 ff ff       	call   4010b0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1806
    printf(" --inf-buffer-size <bytes>      Set the buffer size (default %" PRId64 ").\n", gib_global_inf_init_chunk_size);
  401da8:	48 8b 35 19 33 00 00 	mov    0x3319(%rip),%rsi        # 4050c8 <gib_global_inf_init_chunk_size>
  401daf:	bf 88 30 40 00       	mov    $0x403088,%edi
  401db4:	31 c0                	xor    %eax,%eax
  401db6:	e8 f5 f2 ff ff       	call   4010b0 <printf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1807
    printf(" --bench-input <path>           Set the input file read for benchmarking. Applies only\n");
  401dbb:	bf d0 30 40 00       	mov    $0x4030d0,%edi
  401dc0:	e8 ab f2 ff ff       	call   401070 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1808
    printf("                                If the program was *compiled* with --bench-fun. \n");
  401dc5:	bf 28 31 40 00       	mov    $0x403128,%edi
  401dca:	e8 a1 f2 ff ff       	call   401070 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1809
    printf("\n");
  401dcf:	bf 0a 00 00 00       	mov    $0xa,%edi
  401dd4:	e8 67 f2 ff ff       	call   401040 <putchar@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1810
    printf(" --array-input <path>           Set the file from which to read the array input.\n");
  401dd9:	bf 80 31 40 00       	mov    $0x403180,%edi
  401dde:	e8 8d f2 ff ff       	call   401070 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1811
    printf(" --array-input-length <int>     Set the size of the array input file.\n");
  401de3:	bf d8 31 40 00       	mov    $0x4031d8,%edi
  401de8:	e8 83 f2 ff ff       	call   401070 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1812
    printf(" --iterate <int>                Set the number of timing iterations to perform (default 1).\n");
  401ded:	bf 20 32 40 00       	mov    $0x403220,%edi
  401df2:	e8 79 f2 ff ff       	call   401070 <puts@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1814
    printf(" --size-param <int>             A parameter for size available as a language primitive which allows user to specify the size at runtime (default 1).\n");
  401df7:	bf 80 32 40 00       	mov    $0x403280,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1816
}
  401dfc:	5b                   	pop    %rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1814
    printf(" --size-param <int>             A parameter for size available as a language primitive which allows user to specify the size at runtime (default 1).\n");
  401dfd:	e9 6e f2 ff ff       	jmp    401070 <puts@plt>
  401e02:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  401e09:	00 00 00 00 
  401e0d:	0f 1f 00             	nopl   (%rax)

0000000000401e10 <generate_loop_398_533.part.0>:
generate_loop_398_533():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:99
GibVector *generate_loop_398_533(GibVector *vec_166_578_634,
  401e10:	41 56                	push   %r14
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:108
        GibInt fltPrm_614_641 = n_6_581_637 - idx_167_579_635;
  401e12:	4c 8d 71 ff          	lea    -0x1(%rcx),%r14
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:99
GibVector *generate_loop_398_533(GibVector *vec_166_578_634,
  401e16:	41 55                	push   %r13
  401e18:	49 89 d5             	mov    %rdx,%r13
  401e1b:	41 54                	push   %r12
  401e1d:	49 89 fc             	mov    %rdi,%r12
  401e20:	55                   	push   %rbp
  401e21:	48 89 cd             	mov    %rcx,%rbp
  401e24:	53                   	push   %rbx
  401e25:	48 89 f3             	mov    %rsi,%rbx
  401e28:	48 83 ec 20          	sub    $0x20,%rsp
generate_loop_398_533.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401e2c:	49 8b 3c 24          	mov    (%r12),%rdi
  401e30:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
generate_loop_398_533():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:108
        GibInt fltPrm_614_641 = n_6_581_637 - idx_167_579_635;
  401e35:	48 89 e8             	mov    %rbp,%rax
generate_loop_398_533.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401e38:	48 89 e6             	mov    %rsp,%rsi
generate_loop_398_533():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:108
  401e3b:	48 29 d8             	sub    %rbx,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401e3e:	48 01 df             	add    %rbx,%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:108
  401e41:	48 89 04 24          	mov    %rax,(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  401e45:	48 0f af fa          	imul   %rdx,%rdi
  401e49:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401e4e:	e8 cd f2 ff ff       	call   401120 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:112
        GibInt fltAppE_615_643 = idx_167_579_635 + 1;
  401e53:	48 8d 7b 01          	lea    0x1(%rbx),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:105
    if (fltIf_613_638) {
  401e57:	49 39 fd             	cmp    %rdi,%r13
  401e5a:	74 30                	je     401e8c <generate_loop_398_533.part.0+0x7c>
generate_loop_398_533.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401e5c:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
  401e61:	49 03 3c 24          	add    (%r12),%rdi
generate_loop_398_533():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:108
        GibInt fltPrm_614_641 = n_6_581_637 - idx_167_579_635;
  401e65:	4c 89 f0             	mov    %r14,%rax
generate_loop_398_533.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401e68:	48 8d 74 24 08       	lea    0x8(%rsp),%rsi
generate_loop_398_533():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:108
  401e6d:	48 29 d8             	sub    %rbx,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401e70:	48 0f af fa          	imul   %rdx,%rdi
  401e74:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:108
  401e79:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401e7e:	e8 9d f2 ff ff       	call   401120 <memcpy@plt>
  401e83:	48 8d 7b 02          	lea    0x2(%rbx),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:105
    if (fltIf_613_638) {
  401e87:	49 39 fd             	cmp    %rdi,%r13
  401e8a:	75 14                	jne    401ea0 <generate_loop_398_533.part.0+0x90>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:118
}
  401e8c:	48 83 c4 20          	add    $0x20,%rsp
  401e90:	4c 89 e0             	mov    %r12,%rax
  401e93:	5b                   	pop    %rbx
  401e94:	5d                   	pop    %rbp
  401e95:	41 5c                	pop    %r12
  401e97:	41 5d                	pop    %r13
  401e99:	41 5e                	pop    %r14
  401e9b:	c3                   	ret
  401e9c:	0f 1f 40 00          	nopl   0x0(%rax)
generate_loop_398_533.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401ea0:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
  401ea5:	49 03 3c 24          	add    (%r12),%rdi
generate_loop_398_533():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:108
        GibInt fltPrm_614_641 = n_6_581_637 - idx_167_579_635;
  401ea9:	48 8d 45 fe          	lea    -0x2(%rbp),%rax
generate_loop_398_533.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401ead:	48 8d 74 24 10       	lea    0x10(%rsp),%rsi
generate_loop_398_533():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:108
  401eb2:	48 29 d8             	sub    %rbx,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401eb5:	48 0f af fa          	imul   %rdx,%rdi
  401eb9:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:108
  401ebe:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401ec3:	e8 58 f2 ff ff       	call   401120 <memcpy@plt>
  401ec8:	48 8d 7b 03          	lea    0x3(%rbx),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:105
    if (fltIf_613_638) {
  401ecc:	49 39 fd             	cmp    %rdi,%r13
  401ecf:	74 bb                	je     401e8c <generate_loop_398_533.part.0+0x7c>
generate_loop_398_533.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401ed1:	49 8b 54 24 10       	mov    0x10(%r12),%rdx
  401ed6:	49 03 3c 24          	add    (%r12),%rdi
generate_loop_398_533():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:108
        GibInt fltPrm_614_641 = n_6_581_637 - idx_167_579_635;
  401eda:	48 8d 45 fd          	lea    -0x3(%rbp),%rax
generate_loop_398_533.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401ede:	48 8d 74 24 18       	lea    0x18(%rsp),%rsi
generate_loop_398_533():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:108
  401ee3:	48 29 d8             	sub    %rbx,%rax
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:112
        GibInt fltAppE_615_643 = idx_167_579_635 + 1;
  401ee6:	48 83 c3 04          	add    $0x4,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401eea:	48 0f af fa          	imul   %rdx,%rdi
  401eee:	49 03 7c 24 18       	add    0x18(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:108
        GibInt fltPrm_614_641 = n_6_581_637 - idx_167_579_635;
  401ef3:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401ef8:	e8 23 f2 ff ff       	call   401120 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:105
    if (fltIf_613_638) {
  401efd:	49 39 dd             	cmp    %rbx,%r13
  401f00:	74 8a                	je     401e8c <generate_loop_398_533.part.0+0x7c>
  401f02:	e9 25 ff ff ff       	jmp    401e2c <generate_loop_398_533.part.0+0x1c>
generate_loop_398_533.part.0():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:105
  401f07:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  401f0e:	00 00 

0000000000401f10 <generate_loop_398_536.part.0.isra.0>:
generate_loop_398_536():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:119
GibVector *generate_loop_398_536(GibVector *vec_166_591_649,
  401f10:	41 55                	push   %r13
  401f12:	49 89 d5             	mov    %rdx,%r13
  401f15:	41 54                	push   %r12
  401f17:	49 89 f4             	mov    %rsi,%r12
  401f1a:	55                   	push   %rbp
  401f1b:	48 89 fd             	mov    %rdi,%rbp
  401f1e:	53                   	push   %rbx
  401f1f:	48 89 cb             	mov    %rcx,%rbx
  401f22:	48 83 ec 28          	sub    $0x28,%rsp
generate_loop_398_536.part.0.isra.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401f26:	48 8b 03             	mov    (%rbx),%rax
generate_loop_398_536():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:132
        GibInt fltAppE_618_656 = *tmp_0;
  401f29:	48 8b 53 18          	mov    0x18(%rbx),%rdx
generate_loop_398_536.part.0.isra.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401f2d:	48 89 e6             	mov    %rsp,%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401f30:	48 8b 7d 00          	mov    0x0(%rbp),%rdi
  401f34:	4c 01 e0             	add    %r12,%rax
  401f37:	48 0f af 43 10       	imul   0x10(%rbx),%rax
  401f3c:	4c 01 e7             	add    %r12,%rdi
add1():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:85
    GibInt tailprim_667 = x_10_552_630 + 1;
  401f3f:	48 8b 04 02          	mov    (%rdx,%rax,1),%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  401f43:	48 8b 55 10          	mov    0x10(%rbp),%rdx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:85
  401f47:	48 83 c0 01          	add    $0x1,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  401f4b:	48 0f af fa          	imul   %rdx,%rdi
  401f4f:	48 03 7d 18          	add    0x18(%rbp),%rdi
generate_loop_398_536():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:133
        GibInt fltPrm_617_657 =  add1(fltAppE_618_656);
  401f53:	48 89 04 24          	mov    %rax,(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401f57:	e8 c4 f1 ff ff       	call   401120 <memcpy@plt>
  401f5c:	49 8d 7c 24 01       	lea    0x1(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:125
    if (fltIf_616_653) {
  401f61:	49 39 fd             	cmp    %rdi,%r13
  401f64:	74 40                	je     401fa6 <generate_loop_398_536.part.0.isra.0+0x96>
generate_loop_398_536.part.0.isra.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401f66:	48 8b 03             	mov    (%rbx),%rax
generate_loop_398_536():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:132
        GibInt fltAppE_618_656 = *tmp_0;
  401f69:	48 8b 53 18          	mov    0x18(%rbx),%rdx
generate_loop_398_536.part.0.isra.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401f6d:	48 8d 74 24 08       	lea    0x8(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401f72:	48 01 f8             	add    %rdi,%rax
  401f75:	48 0f af 43 10       	imul   0x10(%rbx),%rax
  401f7a:	48 03 7d 00          	add    0x0(%rbp),%rdi
add1():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:85
    GibInt tailprim_667 = x_10_552_630 + 1;
  401f7e:	48 8b 04 02          	mov    (%rdx,%rax,1),%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  401f82:	48 8b 55 10          	mov    0x10(%rbp),%rdx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:85
  401f86:	48 83 c0 01          	add    $0x1,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  401f8a:	48 0f af fa          	imul   %rdx,%rdi
  401f8e:	48 03 7d 18          	add    0x18(%rbp),%rdi
generate_loop_398_536():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:133
        GibInt fltPrm_617_657 =  add1(fltAppE_618_656);
  401f92:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401f97:	e8 84 f1 ff ff       	call   401120 <memcpy@plt>
  401f9c:	49 8d 7c 24 02       	lea    0x2(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:125
    if (fltIf_616_653) {
  401fa1:	49 39 fd             	cmp    %rdi,%r13
  401fa4:	75 12                	jne    401fb8 <generate_loop_398_536.part.0.isra.0+0xa8>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:143
}
  401fa6:	48 83 c4 28          	add    $0x28,%rsp
  401faa:	5b                   	pop    %rbx
  401fab:	5d                   	pop    %rbp
  401fac:	41 5c                	pop    %r12
  401fae:	41 5d                	pop    %r13
  401fb0:	c3                   	ret
  401fb1:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)
generate_loop_398_536.part.0.isra.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401fb8:	48 8b 03             	mov    (%rbx),%rax
generate_loop_398_536():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:132
        GibInt fltAppE_618_656 = *tmp_0;
  401fbb:	48 8b 53 18          	mov    0x18(%rbx),%rdx
generate_loop_398_536.part.0.isra.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401fbf:	48 8d 74 24 10       	lea    0x10(%rsp),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401fc4:	48 01 f8             	add    %rdi,%rax
  401fc7:	48 0f af 43 10       	imul   0x10(%rbx),%rax
  401fcc:	48 03 7d 00          	add    0x0(%rbp),%rdi
add1():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:85
    GibInt tailprim_667 = x_10_552_630 + 1;
  401fd0:	48 8b 04 02          	mov    (%rdx,%rax,1),%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  401fd4:	48 8b 55 10          	mov    0x10(%rbp),%rdx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:85
  401fd8:	48 83 c0 01          	add    $0x1,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  401fdc:	48 0f af fa          	imul   %rdx,%rdi
  401fe0:	48 03 7d 18          	add    0x18(%rbp),%rdi
generate_loop_398_536():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:133
        GibInt fltPrm_617_657 =  add1(fltAppE_618_656);
  401fe4:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401fe9:	e8 32 f1 ff ff       	call   401120 <memcpy@plt>
  401fee:	49 8d 7c 24 03       	lea    0x3(%r12),%rdi
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:125
    if (fltIf_616_653) {
  401ff3:	49 39 fd             	cmp    %rdi,%r13
  401ff6:	74 ae                	je     401fa6 <generate_loop_398_536.part.0.isra.0+0x96>
generate_loop_398_536.part.0.isra.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  401ff8:	48 8b 03             	mov    (%rbx),%rax
generate_loop_398_536():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:132
        GibInt fltAppE_618_656 = *tmp_0;
  401ffb:	48 8b 53 18          	mov    0x18(%rbx),%rdx
generate_loop_398_536.part.0.isra.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  401fff:	48 8d 74 24 18       	lea    0x18(%rsp),%rsi
generate_loop_398_536():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:137
        GibInt fltAppE_619_659 = idx_167_592_650 + 1;
  402004:	49 83 c4 04          	add    $0x4,%r12
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
    return ((char*)vec->data + (vec->elt_size * (vec->lower + i)));
  402008:	48 01 f8             	add    %rdi,%rax
  40200b:	48 0f af 43 10       	imul   0x10(%rbx),%rax
generate_loop_398_536.part.0.isra.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402010:	48 03 7d 00          	add    0x0(%rbp),%rdi
add1():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:85
    GibInt tailprim_667 = x_10_552_630 + 1;
  402014:	48 8b 04 02          	mov    (%rdx,%rax,1),%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402018:	48 8b 55 10          	mov    0x10(%rbp),%rdx
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:85
  40201c:	48 83 c0 01          	add    $0x1,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:577
  402020:	48 0f af fa          	imul   %rdx,%rdi
  402024:	48 03 7d 18          	add    0x18(%rbp),%rdi
generate_loop_398_536():
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:133
        GibInt fltPrm_617_657 =  add1(fltAppE_618_656);
  402028:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:583
    memcpy(dst, elt, vec->elt_size);
  40202d:	e8 ee f0 ff ff       	call   401120 <memcpy@plt>
/local/scratch/a/singhav/gibbon/gibbon-compiler/examples/tail_recursion/MapOnVector.c:125
    if (fltIf_616_653) {
  402032:	4d 39 e5             	cmp    %r12,%r13
  402035:	0f 84 6b ff ff ff    	je     401fa6 <generate_loop_398_536.part.0.isra.0+0x96>
  40203b:	e9 e6 fe ff ff       	jmp    401f26 <generate_loop_398_536.part.0.isra.0+0x16>

0000000000402040 <gib_vector_alloc.constprop.0>:
gib_vector_alloc.constprop.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:512
GibVector *gib_vector_alloc(GibInt num, size_t elt_size)
  402040:	41 54                	push   %r12
  402042:	53                   	push   %rbx
  402043:	48 89 fb             	mov    %rdi,%rbx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  402046:	bf 20 00 00 00       	mov    $0x20,%edi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:512
GibVector *gib_vector_alloc(GibInt num, size_t elt_size)
  40204b:	48 83 ec 08          	sub    $0x8,%rsp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  40204f:	e8 dc f0 ff ff       	call   401130 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:515
    if (vec == NULL) {
  402054:	48 85 c0             	test   %rax,%rax
  402057:	74 3b                	je     402094 <gib_vector_alloc.constprop.0+0x54>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:519
    void *data = (void *) gib_alloc(num * elt_size);
  402059:	48 8d 3c dd 00 00 00 	lea    0x0(,%rbx,8),%rdi
  402060:	00 
  402061:	49 89 c4             	mov    %rax,%r12
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  402064:	e8 c7 f0 ff ff       	call   401130 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:520
    if (data == NULL) {
  402069:	48 85 c0             	test   %rax,%rax
  40206c:	74 48                	je     4020b6 <gib_vector_alloc.constprop.0+0x76>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:527
    vec->data = data;
  40206e:	49 89 44 24 18       	mov    %rax,0x18(%r12)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:529
}
  402073:	4c 89 e0             	mov    %r12,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:524
    vec->lower = 0;
  402076:	49 c7 04 24 00 00 00 	movq   $0x0,(%r12)
  40207d:	00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:525
    vec->upper = num;
  40207e:	49 89 5c 24 08       	mov    %rbx,0x8(%r12)
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:526
    vec->elt_size = elt_size;
  402083:	49 c7 44 24 10 08 00 	movq   $0x8,0x10(%r12)
  40208a:	00 00 
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:529
}
  40208c:	48 83 c4 08          	add    $0x8,%rsp
  402090:	5b                   	pop    %rbx
  402091:	41 5c                	pop    %r12
  402093:	c3                   	ret
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:516
        fprintf(stderr, "alloc_vector: gib_alloc failed: %zu", sizeof(GibVector));
  402094:	48 8b 3d 65 30 00 00 	mov    0x3065(%rip),%rdi        # 405100 <stderr@GLIBC_2.2.5>
  40209b:	ba 20 00 00 00       	mov    $0x20,%edx
  4020a0:	be 18 33 40 00       	mov    $0x403318,%esi
  4020a5:	31 c0                	xor    %eax,%eax
  4020a7:	e8 64 f0 ff ff       	call   401110 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:517
        exit(1);
  4020ac:	bf 01 00 00 00       	mov    $0x1,%edi
  4020b1:	e8 ba f0 ff ff       	call   401170 <exit@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:521
        fprintf(stderr, "alloc_vector: gib_alloc failed: %zu", sizeof(num * elt_size));
  4020b6:	48 8b 3d 43 30 00 00 	mov    0x3043(%rip),%rdi        # 405100 <stderr@GLIBC_2.2.5>
  4020bd:	ba 08 00 00 00       	mov    $0x8,%edx
  4020c2:	be 18 33 40 00       	mov    $0x403318,%esi
  4020c7:	e8 44 f0 ff ff       	call   401110 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:522
        exit(1);
  4020cc:	bf 01 00 00 00       	mov    $0x1,%edi
  4020d1:	e8 9a f0 ff ff       	call   401170 <exit@plt>
  4020d6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4020dd:	00 00 00 

00000000004020e0 <check_args.part.0>:
check_args.part.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1876
void check_args(int i, int argc, char **argv, char *parameter){
  4020e0:	55                   	push   %rbp
  4020e1:	48 89 fd             	mov    %rdi,%rbp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1878
        fprintf(stderr, "Not enough arguments after %s, expected <int>.\n", parameter);
  4020e4:	48 8b 3d 15 30 00 00 	mov    0x3015(%rip),%rdi        # 405100 <stderr@GLIBC_2.2.5>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1876
void check_args(int i, int argc, char **argv, char *parameter){
  4020eb:	48 89 f2             	mov    %rsi,%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1878
        fprintf(stderr, "Not enough arguments after %s, expected <int>.\n", parameter);
  4020ee:	31 c0                	xor    %eax,%eax
  4020f0:	be 40 33 40 00       	mov    $0x403340,%esi
  4020f5:	e8 16 f0 ff ff       	call   401110 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1879
        gib_show_usage(argv);
  4020fa:	48 89 ef             	mov    %rbp,%rdi
  4020fd:	e8 4e fc ff ff       	call   401d50 <gib_show_usage>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1880
        exit(1);
  402102:	bf 01 00 00 00       	mov    $0x1,%edi
  402107:	e8 64 f0 ff ff       	call   401170 <exit@plt>
  40210c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402110 <gib_shadowstack_initialize.part.0.constprop.0>:
gib_shadowstack_initialize.part.0.constprop.0():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1483
static void gib_shadowstack_initialize(GibShadowstack* stack, size_t stack_size)
  402110:	48 83 ec 08          	sub    $0x8,%rsp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1487
        fprintf(stderr, "gib_shadowstack_initialize: gib_alloc failed: %zu",
  402114:	ba 00 00 00 06       	mov    $0x6000000,%edx
  402119:	be 70 33 40 00       	mov    $0x403370,%esi
  40211e:	31 c0                	xor    %eax,%eax
  402120:	48 8b 3d d9 2f 00 00 	mov    0x2fd9(%rip),%rdi        # 405100 <stderr@GLIBC_2.2.5>
  402127:	e8 e4 ef ff ff       	call   401110 <fprintf@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1489
        exit(1);
  40212c:	bf 01 00 00 00       	mov    $0x1,%edi
  402131:	e8 3a f0 ff ff       	call   401170 <exit@plt>
  402136:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40213d:	00 00 00 

0000000000402140 <gib_check_rust_struct_sizes>:
gib_check_rust_struct_sizes():
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:994
{
  402140:	55                   	push   %rbp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  402141:	bf 38 00 00 00       	mov    $0x38,%edi
  402146:	e8 e5 ef ff ff       	call   401130 <malloc@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1004
    gib_get_rust_struct_sizes(stack, frame, nursery, generation, reg_info, footer, gc_stats);
  40214b:	48 83 ec 08          	sub    $0x8,%rsp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:241
void *gib_alloc(size_t n) { return malloc(n); }
  40214f:	48 89 c5             	mov    %rax,%rbp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:999
    nursery = (size_t *) ((char *) frame + sizeof(size_t));
  402152:	48 8d 50 10          	lea    0x10(%rax),%rdx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1000
    generation = (size_t *) ((char *) nursery + sizeof(size_t));
  402156:	48 8d 48 18          	lea    0x18(%rax),%rcx
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:998
    frame = (size_t *) ((char *) stack + sizeof(size_t));
  40215a:	48 8d 70 08          	lea    0x8(%rax),%rsi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1003
    gc_stats = (size_t *) ((char *) footer + sizeof(size_t));
  40215e:	48 83 c0 30          	add    $0x30,%rax
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1004
    gib_get_rust_struct_sizes(stack, frame, nursery, generation, reg_info, footer, gc_stats);
  402162:	4c 8d 4d 28          	lea    0x28(%rbp),%r9
  402166:	48 89 ef             	mov    %rbp,%rdi
  402169:	50                   	push   %rax
  40216a:	4c 8d 45 20          	lea    0x20(%rbp),%r8
  40216e:	e8 dd ef ff ff       	call   401150 <gib_get_rust_struct_sizes@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1007
    assert(*stack == sizeof(GibShadowstack));
  402173:	48 83 7d 00 18       	cmpq   $0x18,0x0(%rbp)
  402178:	58                   	pop    %rax
  402179:	5a                   	pop    %rdx
  40217a:	75 42                	jne    4021be <gib_check_rust_struct_sizes+0x7e>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1008
    assert(*frame == sizeof(GibShadowstackFrame));
  40217c:	48 83 7d 08 18       	cmpq   $0x18,0x8(%rbp)
  402181:	0f 85 cd 00 00 00    	jne    402254 <gib_check_rust_struct_sizes+0x114>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1009
    assert(*nursery == sizeof(GibNursery));
  402187:	48 83 7d 10 20       	cmpq   $0x20,0x10(%rbp)
  40218c:	0f 85 a9 00 00 00    	jne    40223b <gib_check_rust_struct_sizes+0xfb>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1010
    assert(*generation == sizeof(GibOldgen));
  402192:	48 83 7d 18 18       	cmpq   $0x18,0x18(%rbp)
  402197:	0f 85 85 00 00 00    	jne    402222 <gib_check_rust_struct_sizes+0xe2>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1011
    assert(*reg_info == sizeof(GibRegionInfo));
  40219d:	48 83 7d 20 20       	cmpq   $0x20,0x20(%rbp)
  4021a2:	75 65                	jne    402209 <gib_check_rust_struct_sizes+0xc9>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1012
    assert(*footer == sizeof(GibOldgenChunkFooter));
  4021a4:	48 83 7d 28 18       	cmpq   $0x18,0x28(%rbp)
  4021a9:	75 45                	jne    4021f0 <gib_check_rust_struct_sizes+0xb0>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1013
    assert(*gc_stats == sizeof(GibGcStats));
  4021ab:	48 81 7d 30 f0 00 00 	cmpq   $0xf0,0x30(%rbp)
  4021b2:	00 
  4021b3:	75 22                	jne    4021d7 <gib_check_rust_struct_sizes+0x97>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:242
void gib_free(void *ptr) { free(ptr); }
  4021b5:	48 89 ef             	mov    %rbp,%rdi
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1019
}
  4021b8:	5d                   	pop    %rbp
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:242
void gib_free(void *ptr) { free(ptr); }
  4021b9:	e9 72 ee ff ff       	jmp    401030 <free@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1007 (discriminator 1)
    assert(*stack == sizeof(GibShadowstack));
  4021be:	b9 60 37 40 00       	mov    $0x403760,%ecx
  4021c3:	ba ef 03 00 00       	mov    $0x3ef,%edx
  4021c8:	be a8 33 40 00       	mov    $0x4033a8,%esi
  4021cd:	bf e8 33 40 00       	mov    $0x4033e8,%edi
  4021d2:	e8 f9 ee ff ff       	call   4010d0 <__assert_fail@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1013 (discriminator 1)
    assert(*gc_stats == sizeof(GibGcStats));
  4021d7:	b9 60 37 40 00       	mov    $0x403760,%ecx
  4021dc:	ba f5 03 00 00       	mov    $0x3f5,%edx
  4021e1:	be a8 33 40 00       	mov    $0x4033a8,%esi
  4021e6:	bf d0 34 40 00       	mov    $0x4034d0,%edi
  4021eb:	e8 e0 ee ff ff       	call   4010d0 <__assert_fail@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1012 (discriminator 1)
    assert(*footer == sizeof(GibOldgenChunkFooter));
  4021f0:	b9 60 37 40 00       	mov    $0x403760,%ecx
  4021f5:	ba f4 03 00 00       	mov    $0x3f4,%edx
  4021fa:	be a8 33 40 00       	mov    $0x4033a8,%esi
  4021ff:	bf a8 34 40 00       	mov    $0x4034a8,%edi
  402204:	e8 c7 ee ff ff       	call   4010d0 <__assert_fail@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1011 (discriminator 1)
    assert(*reg_info == sizeof(GibRegionInfo));
  402209:	b9 60 37 40 00       	mov    $0x403760,%ecx
  40220e:	ba f3 03 00 00       	mov    $0x3f3,%edx
  402213:	be a8 33 40 00       	mov    $0x4033a8,%esi
  402218:	bf 80 34 40 00       	mov    $0x403480,%edi
  40221d:	e8 ae ee ff ff       	call   4010d0 <__assert_fail@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1010 (discriminator 1)
    assert(*generation == sizeof(GibOldgen));
  402222:	b9 60 37 40 00       	mov    $0x403760,%ecx
  402227:	ba f2 03 00 00       	mov    $0x3f2,%edx
  40222c:	be a8 33 40 00       	mov    $0x4033a8,%esi
  402231:	bf 58 34 40 00       	mov    $0x403458,%edi
  402236:	e8 95 ee ff ff       	call   4010d0 <__assert_fail@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1009 (discriminator 1)
    assert(*nursery == sizeof(GibNursery));
  40223b:	b9 60 37 40 00       	mov    $0x403760,%ecx
  402240:	ba f1 03 00 00       	mov    $0x3f1,%edx
  402245:	be a8 33 40 00       	mov    $0x4033a8,%esi
  40224a:	bf 38 34 40 00       	mov    $0x403438,%edi
  40224f:	e8 7c ee ff ff       	call   4010d0 <__assert_fail@plt>
/local/scratch/a/singhav/gibbon/gibbon-rts/rts-c/gibbon_rts.c:1008 (discriminator 1)
    assert(*frame == sizeof(GibShadowstackFrame));
  402254:	b9 60 37 40 00       	mov    $0x403760,%ecx
  402259:	ba f0 03 00 00       	mov    $0x3f0,%edx
  40225e:	be a8 33 40 00       	mov    $0x4033a8,%esi
  402263:	bf 10 34 40 00       	mov    $0x403410,%edi
  402268:	e8 63 ee ff ff       	call   4010d0 <__assert_fail@plt>

Disassembly of section .fini:

0000000000402270 <_fini>:
_fini():
  402270:	f3 0f 1e fa          	endbr64
  402274:	48 83 ec 08          	sub    $0x8,%rsp
  402278:	48 83 c4 08          	add    $0x8,%rsp
  40227c:	c3                   	ret
