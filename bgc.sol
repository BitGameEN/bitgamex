pragma solidity ^0.4.18;

interface tokenRecipient { function receiveApproval(address _from, uint256 _value, address _token, bytes _extraData) external; }

contract BGCToken {
    // 以下参数测试时会临时修改，在正式发布时需要修正为正式参数 ======>
    string public name = "BitGameCoin1";
    string public symbol = "BGC1";

    uint256 baseTokenPerETH = 50000; // 无优惠时的兑换比率

    address ethFundAddress  = 0x0; //以太坊轉存地址
    address[] foundationAddresses = [0x0]; // 基金会地址，15个
    address[] teamAddresses = [0x0]; // 团队地址，15个
    address[] miningAddresses = [0x0]; // 挖矿地址，2个
    address[] cornerstoneAddresses = [0x0]; // 基石私募地址，10个
    address[] preIcoAddresses = [0x0]; // PreICO地址，5个

    uint256 startTime = 1522135518; // 开始时间戳，UTC-0
    uint256 stage1Seconds = 600; // 10% bonus
    uint256 stage2Seconds = 600; // 5% bonus
    uint256 stage3Seconds = 600; // 0 bonus
    uint256 stage4Seconds = 600; // -5% bonus
    uint256 stage5Seconds = 600; // -10% bonus

    //uint256 startTime = 1525017600; // 开始时间戳，2018/4/30 0:0:0 UTC-0
    //uint256 stage1Seconds = 3 * 24 * 3600; // 10% bonus，4.30 - 5.2
    //uint256 stage2Seconds = 4 * 24 * 3600; // 5% bonus，5.3 - 5.6
    //uint256 stage3Seconds = 10 * 24 * 3600; // 0 bonus，5.7 - 5.16
    //uint256 stage4Seconds = 8 * 24 * 3600; // -5% bonus，5.17 - 5.24
    //uint256 stage5Seconds = 6 * 24 * 3600; // -10% bonus，5.25 - 5.30
    // <====== 正式发布需要修正的参数


    uint256 public stage1EndTime = startTime + stage1Seconds;
    uint256 public stage2EndTime = stage1EndTime + stage2Seconds;
    uint256 public stage3EndTime = stage2EndTime + stage3Seconds;
    uint256 public stage4EndTime = stage3EndTime + stage4Seconds;
    uint256 public endTime = stage4EndTime + stage5Seconds; // 结束时间戳

    uint256 public decimals = 18;
    uint256 DECIMALSFACTOR = 10 ** decimals;
    uint256 constant weiDECIMALS = 18; // 以太币的小数位
    uint256 weiFACTOR = 10 ** weiDECIMALS; // 以太币的单位换算值

    uint256 reservedAmountPerFoundationAddress = 1 * (10**8) * DECIMALSFACTOR;// 轉移1亿
    uint256 reservedAmountPerTeamAddress = 1 * (10**8) * DECIMALSFACTOR;// 轉移1亿
    uint256 reservedAmountPerMiningAddress = 15 * (10**8) * DECIMALSFACTOR;// 轉移15亿
    uint256 reservedAmountPerCornerstoneAddress = 1 * (10**8) * DECIMALSFACTOR;// 轉移1亿
    uint256 reservedAmountPerPreIcoAddress = 1 * (10**8) * DECIMALSFACTOR;// 轉移1亿

    uint256 public currentTokenPerETH = baseTokenPerETH;

    address contractOwner;
    uint256 ethRaised; // 收到的ETH总数量，单位Wei
    uint256 tokenDistributed; // 分发出去的Token数量
    uint256 donationCount; // 参与的总次数

    uint256 public totalSupply = 100 * (10**8) * DECIMALSFACTOR; // 总量100亿
    uint256 public availableSupply = totalSupply; // 剩余的代币数量
    uint256 softCap = 5.5 * (10**8) * DECIMALSFACTOR; // 软顶5.5亿代币
    uint256 minimumDonation = 1 * 10 ** (weiDECIMALS - 1); // 最低参与0.1ETH才能参与

    uint8 public currentStage = 0;
    bool public isInLockStage = true;
    bool public finalised = false;

    // 存储所有用户的代币余额值
    mapping (address => uint256) public balanceOf;
    mapping (address => mapping (address => uint256)) public allowance;

    // This generates a public event on the blockchain that will notify clients
    event Transfer(address indexed from, address indexed to, uint256 value);

    // This notifies clients about the amount burnt
    event Burn(address indexed from, uint256 value);

    function BGCToken() public {
        contractOwner = msg.sender;

        // 采用累加方式，防止有地址重复
        uint i = 0;
        for (i = 0; i < foundationAddresses.length; i++){
            balanceOf[foundationAddresses[i]] += reservedAmountPerFoundationAddress;
            availableSupply -= reservedAmountPerFoundationAddress;
        }
        for (i = 0; i < teamAddresses.length; i++){
            balanceOf[teamAddresses[i]] += reservedAmountPerTeamAddress;
            availableSupply -= reservedAmountPerTeamAddress;
        }
        for (i = 0; i < miningAddresses.length; i++){
            balanceOf[miningAddresses[i]] += reservedAmountPerMiningAddress;
            availableSupply -= reservedAmountPerMiningAddress;
        }
        for (i = 0; i < cornerstoneAddresses.length; i++){
            balanceOf[cornerstoneAddresses[i]] += reservedAmountPerCornerstoneAddress;
            availableSupply -= reservedAmountPerCornerstoneAddress;
        }
        for (i = 0; i < preIcoAddresses.length; i++){
            balanceOf[preIcoAddresses[i]] += reservedAmountPerPreIcoAddress;
            availableSupply -= reservedAmountPerPreIcoAddress;
        }

        balanceOf[contractOwner] = availableSupply; // 剩下的代币初始都存在创建合约的地址上
    }

    // fallback方法，如果用户未在转账data中添加数据，默认是走这个方法
    function () payable public {
        require(!finalised);

        // 判断是否在项目规定的时间范围内
        require(block.timestamp >= startTime);
        require(block.timestamp <= endTime);

        // 判断代币是否已经分发完毕
        require(availableSupply > 0);

        // 分配代币
        require(msg.value >= minimumDonation); // 达到最低捐赠额度才能继续，否则失败

        if (block.timestamp <= stage1EndTime){
            currentStage = 1;
            currentTokenPerETH = baseTokenPerETH + baseTokenPerETH / 10;
        }else if (block.timestamp <= stage2EndTime){
            currentStage = 2;
            currentTokenPerETH = baseTokenPerETH + baseTokenPerETH / 20;
        }else if (block.timestamp <= stage3EndTime){
            currentStage = 3;
            currentTokenPerETH = baseTokenPerETH;
        }else if (block.timestamp <= stage4EndTime){
            currentStage = 4;
            currentTokenPerETH = baseTokenPerETH - baseTokenPerETH / 20;
        }else{
            currentStage = 5;
            currentTokenPerETH = baseTokenPerETH - baseTokenPerETH / 10;
        }

        // 计算该地址本次参与所获得的Token数量
        uint256 etherValue = msg.value;
        uint256 tokenValue = currentTokenPerETH * etherValue / 10 ** (weiDECIMALS - decimals);

        // 边界条件，未超过部分的ETH正常计算相应代币，超过的部分退回给用户
        if (tokenValue > availableSupply){
            tokenValue = availableSupply;
            // 单位为wei
            etherValue = weiFACTOR * availableSupply / currentTokenPerETH / DECIMALSFACTOR;
            // 超过的部分退回给用户
            assert(msg.value > etherValue);
            msg.sender.transfer(msg.value - etherValue);
        }

        // 调用内部转账方法给该地址转相应数量的Token
        _transfer(contractOwner, msg.sender, tokenValue);

        // 转移ETH到指定ETH存币地址
        ethFundAddress.transfer(etherValue);

        donationCount += 1;
        ethRaised += etherValue;
        availableSupply -= tokenValue;
        tokenDistributed += tokenValue;
        if (tokenDistributed >= softCap){
            isInLockStage = false;
        }else{
            isInLockStage = true;
        }
    }

    /**
     * Internal transfer, only can be called by this contract
     */
    function _transfer(address _from, address _to, uint _value) internal {
        // Prevent transfer to 0x0 address. Use burn() instead
        require(_to != 0x0);
        // Check if the sender has enough
        require(balanceOf[_from] >= _value);
        // Check for overflows
        require(balanceOf[_to] + _value > balanceOf[_to]);
        // Save this for an assertion in the future
        uint previousBalances = balanceOf[_from] + balanceOf[_to];
        // Subtract from the sender
        balanceOf[_from] -= _value;
        // Add the same to the recipient
        balanceOf[_to] += _value;
        emit Transfer(_from, _to, _value);
        // Asserts are used to use static analysis to find bugs in your code. They should never fail
        assert(balanceOf[_from] + balanceOf[_to] == previousBalances);
    }

    function transfer(address _to, uint256 _value) public {
        require(!isInLockStage);
        _transfer(msg.sender, _to, _value);
    }

    function transferFrom(address _from, address _to, uint256 _value) public returns (bool success) {
        require(_value <= allowance[_from][msg.sender]);     // Check allowance
        allowance[_from][msg.sender] -= _value;
        _transfer(_from, _to, _value);
        return true;
    }

    function approve(address _spender, uint256 _value) public returns (bool success) {
        allowance[msg.sender][_spender] = _value;
        return true;
    }

    function approveAndCall(address _spender, uint256 _value, bytes _extraData) public returns (bool success) {
        tokenRecipient spender = tokenRecipient(_spender);
        if (approve(_spender, _value)) {
            spender.receiveApproval(msg.sender, _value, this, _extraData);
            return true;
        }
    }

    function burn(uint256 _value) public returns (bool success) {
        require(balanceOf[msg.sender] >= _value);   // Check if the sender has enough
        balanceOf[msg.sender] -= _value;            // Subtract from the sender
        totalSupply -= _value;                      // Update totalSupply
        emit Burn(msg.sender, _value);
        return true;
    }

    function burnFrom(address _from, uint256 _value) public returns (bool success) {
        require(balanceOf[_from] >= _value);                // Check if the targeted balance is enough
        require(_value <= allowance[_from][msg.sender]);    // Check allowance
        balanceOf[_from] -= _value;                         // Subtract from the targeted balance
        allowance[_from][msg.sender] -= _value;             // Subtract from the sender's allowance
        totalSupply -= _value;                              // Update totalSupply
        emit Burn(_from, _value);
        return true;
    }

    function finalise() public {
        require(msg.sender == contractOwner);
        require(!finalised);

        finalised = true;
    }

    //手工解除代币锁定
    function unlockTokens() public {
        require(msg.sender == contractOwner);

        isInLockStage = false;
    }

    function tokenHasDistributed() public constant returns (uint256) {
        return tokenDistributed;
    }
}
