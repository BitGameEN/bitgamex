using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ServerVO{
	public class LoginGameVO: ErrorArg{
		public int succ;
		public int uid;
		public string token;
		public string game_data;
		public float role_balance;
		public float user_balance;
		public string exchange_accid;
		public string wallet_addr;
	}

	public class GetGameVO: ErrorArg{
		public int succ;
		public string game_data;
		public float role_balance;
	}

	public class SaveGameVO: ErrorArg{
		public int succ;
		public string game_data;
		public float role_balance;
		public float f_res;
	}

	public class TransferCoinInGameVO: ErrorArg{
		public int succ;
		public float role_balance;
	}

	public class BindExchangeAccidVO: ErrorArg{
		public int succ;
		public string exchange_accid;
	}

	public class TransferCoinToExchangeVO: ErrorArg{
		public int succ;
		public float role_balance;
		public float exchange_balance;
	}

	public class BindWalletVO: ErrorArg{
		public int succ;
		public string wallet_addr;
	}

	public class TransferCoinToWalletVO: ErrorArg{
		public int succ;
		public float role_balance;
		public float exchange_balance;
	}

	public class ErrorArg{
		public string req;
		public int errno;
		public string errmsg;
		public enum ErrorNo:int{
			ERROR_IP = -999,
			LACK_ARGS = -998,
			ERROR_ARGS = -997,
			CHECK_FAIL = -996,
			PROTOCOL_UNSUPPORT = -995,
			THROW_EXCEPTION = -994,
			GAME_NOT_OPEN = -993,
			READ_CACHE_FAIL = -992,
			NO_RPC_SERVER = -991,
			GOLD_NOT_ENOUGH = -990,
			REQUEST_FAILED = -989,
			REQUEST_TIMEOUT = -988,
			UNKNOWN = -987
		}
	}
}
