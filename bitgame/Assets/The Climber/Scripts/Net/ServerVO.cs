using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ServerVO{
	public class LoginGameVO{
		public int succ;
		public int uid;
		public string token;
		public string game_data;
		public float balance;
	}

	public class GetGameVO{
		public int succ;
		public string game_data;
		public float balance;
	}

	public class SaveGameVO{
		public int succ;
		public string game_data;
		public float balance;
		public float f_res;
	}

	public class TransferCoinInGameVO{
		public int succ;
		public float balance;
	}

	public class BindExchangeAccidVO{
		public int succ;
		public string exchange_accid;
	}

	public class TransferCoinToExchangeVO{
		public int succ;
		public float balance;
		public float exchange_balance;
	}

	public class BindWalletVO{
		public int succ;
		public string wallet_addr;
	}

	public class TransferCoinToWalletVO{
		public int succ;
		public float balance;
		public float exchange_balance;
	}

	public class ErrorArg{
		public int succ;
		public string req;
		public int errorno;
		public string errmsg;
		public enum ErrorNo:int{
			ERROR_IP = -999,
			LACK_ARGS = -998,
			ERROR_ARGS = -997,
			CHECK_FAIL = -996,
			PROTOCOL_UNSUPPORT = -995,
			THROW_EXCEPTION = -994,
			GAME_NOT_OPEN = -993,
			READ_BUFF_FAIL = -992,
			NO_RPC_SERVER = -991
		}
	}
}
