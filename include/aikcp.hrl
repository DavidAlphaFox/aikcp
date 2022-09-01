-define(KCP_RTO_NDL, 30).
-define(KCP_RTO_MIN, 100).
-define(KCP_RTO_DEF, 200).
-define(KCP_RTO_MAX, 60000).

-define(KCP_CMD_PUSH, 81).
-define(KCP_CMD_ACK, 82).
-define(KCP_CMD_WASK, 83).
-define(KCP_CMD_WINS, 84).

-define(KCP_ASK_SEND, 1).        %% need to send KCP_CMD_WASK
-define(KCP_ASK_TELL, 2).        %% need to send KCP_CMD_WINS

-define(KCP_WND_SND, 32).
-define(KCP_WND_RCV, 128).
-define(KCP_MTU_DEF, 1400).
-define(KCP_ACK_FAST, 3).
-define(KCP_INTERVAL, 100).
-define(KCP_OVERHEAD, 24).
-define(KCP_DEADLINK, 20).
-define(KCP_THRESH_INIT, 2).
-define(KCP_THRESH_MIN, 2).
-define(KCP_PROBE_INIT, 7000).    %% 7 secs to probe window size
-define(KCP_PROBE_LIMIT, 120000). %% up to 120 secs to probe window
-define(KCP_FASTACK_LIMIT,5).
-define(KCP_UPDATE_INTERVAL, 50).

-define(KCP_STATE_ACTIVE, 1).
-define(KCP_STATE_DEAD, -1).

-define(BIT_32(I),((I) band 16#FFFFFFFF)).
-define(BIT_16(I),((I) band 16#FFFF)).

-define(MIN(F, S), case F < S of true -> F; false -> S end).
-define(MAX(F, S), case F < S of true -> S; false -> F end).


%% 循环比较
%% 如果L比R小，结果为负数
%% 如果L比R大，结果为正数
-define(DIFF_32(L,R),(((R - L) band 16#FFFFFFFF) - ((L - R) band 16#FFFFFFFF))).
-define(DIFF_16(L,R),(((R - L) band 16#FFFF) - ((L - R) band 16#FFFF))).





-record(aikcp_seg, {conv = 0, %会话编号，两方一致才能通信
                    cmd = 0, %指令类型，可以同时有多个指令通过与操作设置进来
                    frg = 0, % 分片的编号，当输出数据大于 MSS 时，需要将数据进行分片，frg 记录了分片时的倒序序号
                    wnd = 0, %本方可用窗口大小
                    ts = 0, %当前时间
                    sn = 0, %为 data 报文的编号或者 ack 报文的确认编号；
                    una = 0, %为当前还未确认的数据包的编号
                    len = 0, %数据大小
                    resendts = 0, % 重传时间戳，超过这个时间重发这个包
                    rto = 0,
                    fastack = 0, %快速应答数量，记录被跳过的次数，统计在这个封包的序列号之前有多少报已经应答了。
                                 %比如1，2，3三个封包，收到2的时候知道1被跳过了，此时1的fastack加一，收到3的时候继续加一，超过一定阈值直接重传1这个封包。
                    xmit = 0, %重传次数
                    data = <<>> }).

-record(aikcp_pcb, {conv = 0,
                    mtu = ?KCP_MTU_DEF,
                    mss = ?KCP_MTU_DEF - ?KCP_OVERHEAD,
                    state = 0,
                    snd_una = 0, %最小的未ack序列号，即这个编号前面的所有报都收到了的标志
                    snd_next = 0, %下一个待发送的序列号
                    rcv_next = 0, %下一个待接收的序列号，会通过包头中的una字段通知对端
                    ts_recent = 0,
                    ts_laskack = 0,
                    ssthresh = ?KCP_THRESH_INIT, %slow start threshhold，慢启动阈值
                    rx_rttval = 0, %RTT的平均偏差
                    rx_srtt = 0,  % RTT的一个加权RTT平均值，平滑值。
                    rx_rto = ?KCP_RTO_DEF,
                    rx_minrto = ?KCP_RTO_MIN,
                    snd_wnd = ?KCP_WND_SND,
                    rcv_wnd = ?KCP_WND_RCV,
                    rmt_wnd = ?KCP_WND_RCV, % 对端（rmt=remote）窗口
                    cwnd = ?KCP_WND_SND,
                    probe = 0, % 存储探测标志位
                    current = 0,
                    interval = ?KCP_INTERVAL,
                    ts_flush = ?KCP_INTERVAL,
                    xmit = 0,
                    nodelay = 0,
                    updated = false,
                    ts_probe = 0,
                    probe_wait = 0,
                    dead_link = ?KCP_DEADLINK,
                    incr = 0,
                    snd_queue = aikcp_queue:new(),
                    rcv_queue = aikcp_queue:new(),
                    snd_buf = aikcp_buffer:new(?KCP_WND_SND),
                    rcv_buf = aikcp_buffer:new(?KCP_WND_RCV),
                    acklist = [], %当收到一个数据报文时，将其对应的 ACK 报文的 sn 号以及时间戳 ts
                                  %同时加入到acklist 中，即形成如 [sn1, ts1, sn2, ts2 …] 的列表
                    ackcount = 0, % 记录 acklist 中存放的 ACK 报文的数量
                    %ackblock = 0, % acklist 数组的可用长度，当 acklist 的容量不足时，需要进行扩容
                    fastresend = 0,
                    nocwnd = 0,
                    stream = true,
                    fastlimit = ?KCP_FASTACK_LIMIT,
                    datalist = []}).

-define(KCP_SEG(Conv, Cmd, Frg, Wnd, Ts, Sn, Una, Len, Data, Left),
  <<Conv:32/little, Cmd:8/little, Frg:8/little, Wnd:16/little,
    Ts:32/little, Sn:32/little, Una:32/little, Len:32/little,
    Data:Len/binary, Left/binary>>).

-define(KCP_CONV(Conv),<<Conv:32/little,_/binary>>).
