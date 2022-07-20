#lang scribble/manual
@require[@for-label[qcr
                    racket/base]]

@title{qcr}
@author{haozhang}

@defmodule[qcr]

A small chat room on tcp protocol.

@section{License}

This program is released under LGPL-3.0-only.

@section{简介}

qcr是一个实验性的网络通信程序，可以实现聊天以及文件、目录和链接的共享且性能尚可。

@section{安装使用}

[1]已经安装好Racket。

@itemlist[#:style 'ordered
          @item{`raco pkg install qcr`}
          @item{`racket -e "(dynamic-require '(submod qcr main) #f)" -- [命令行参数]`}]

[2]使用编译好的可分发包。

解压并进入main可执行程序所在目录并执行命令即可。使用`./main --help`查看使用说明。

@section{工作流程}

@itemlist[@item{通过命令行参数设置昵称、主机、端口、模式。}
          @item{建立TCP连接。}
          @item{交换RSA公钥，并使用RSA加密交换vigenere密钥。}
          @item{开始聊天，数据使用vigenere算法加密。}
          @item{聊天结束，Ctrl-c退出。}]

@section{协议}

传输层使用TCP协议，不赘述。

应用层协议基于Racket port构建。

@itemlist[@item{每一个数据包都包含数据包类型、校验和、MD5校验码和实际数据四个部分。}
          @item{MD5校验码和实际数据均经过vigenere加密。}
          @item{数据包有message、link、file、dir四种类型。}]

@section{数据包生成和处理}

@itemlist[@item{数据包的生成和处理理应相互独立以便分为两个Racket thread并发执行，但由于两个过程中都需要从标准输入中读取数据，因此不得不使用sync置于同一Racket thread中执行。}
          @item{程序针对每一行指令或消息只生成一个数据包。}
          @item{message类型数据包实际数据已经经过格式化，解密后直接输出。}
          @item{link类型数据包处理时提供可选的重定向到浏览器的功能，但运行时并不会检查此功能是否可用。}
          @item{file和dir两种类型数据包的实际数据部分包含其名称，默认下载位置为当前目录下的file目录。}
          @item{dir类型数据包生成时首先将目录打包为Zip归档文件，再对文件进行传输，接收端处理时直接以文件形式保存。}]

因此，

@itemlist[@item{不推荐使用此程序分享大的文件或目录，除非你的目的就在于传输数据。}
          @item{不建议以过高的频率输入指令或消息。}]

@section{TODO}

以下计划无先后顺序。

@itemlist[@item{支持加载json配置文件。}
          @item{所有选择操作在y/n外提供a选项。}
          @item{支持历史记录。}
          @item{制作简单UI以更好支持Windows。}]