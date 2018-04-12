using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class API{
	private static API _instance;
	public static API Instance{
		get{
			if(_instance == null){
				_instance = new API();
			}
			return _instance;
		}
	}
	private static string _coin_type = "BGX";

	public void LoginGame(Action success,Action<ServerVO.LoginGameVO> fail){
		var args = new Dictionary<string,object>();
		args["a"] = "login_game";
		args["uid"] = 0;
		args["game_id"] = App.Instance.game.game_id;
		args["device_id"] = App.Instance.device_id;
		args["time"] = App.Instance.time;
		args["device_model"] = App.Instance.device_model;
		args["os_type"] = App.Instance.os_type;
		args["os_ver"] = App.Instance.os_ver;
		args["lang"] = App.Instance.lang;
		args["org_device_id"] = App.Instance.org_device_id;
		args["gc_id"] = App.Instance.gc_id;
		args["fb_id"] = App.Instance.fb_id;
		args["sign"] = Encrypt.Md5(0+""+App.Instance.game.game_id+""+App.Instance.device_id+""+args["time"]+""+App.key);
		Server.Instance.Get<ServerVO.LoginGameVO>(args,(receive)=>{
			if(receive.succ == 1){
				App.Instance.uid = receive.uid;
				App.Instance.token = receive.token;
				App.Instance.game.data = receive.game_data;
				App.Instance.role_balance = receive.role_balance;
				App.Instance.user_balance = receive.user_balance;
				App.Instance.exchange_accid = receive.exchange_accid;
				App.Instance.wallet_addr = receive.wallet_addr;
				success();
			}else{
				fail(receive);
			}
		},()=>{
			fail(null);
		});
	}

	public void GetGame(Action success,Action<ServerVO.GetGameVO> fail){
		var args = new Dictionary<string,object>();
		args["a"] = "get_game";
		args["uid"] = App.Instance.uid;
		args["game_id"] = App.Instance.game.game_id;
		args["token"] = App.Instance.token;
		Server.Instance.Get<ServerVO.GetGameVO>(args,(receive)=>{
			if(receive.succ == 1){
				App.Instance.game.data = receive.game_data;
				App.Instance.role_balance = receive.role_balance;
				success();
			}else{
				fail(receive);
			}
		},()=>{
			fail(null);
		});
	}

	public void SaveGame(Action success,Action<ServerVO.SaveGameVO> fail){
		var args = new Dictionary<string,object>();
		args["a"] = "save_game";
		args["uid"] = App.Instance.uid;
		args["game_id"] = App.Instance.game.game_id;
		args["token"] = App.Instance.token;
		args["game_data"] = App.Instance.game.data;
		args["time"] = App.Instance.time;
		args["sign"] = Encrypt.Md5(App.Instance.uid+""+App.Instance.game.game_id+""+App.Instance.token+App.Instance.game.data+args["time"]+App.key);
		Server.Instance.Post<ServerVO.SaveGameVO>(args,(receive)=>{
			if(receive.succ == 1){
				App.Instance.game.data = receive.game_data;
				App.Instance.role_balance = receive.role_balance;
				Debug.LogWarning("f_res"+receive.f_res);
				success();
			}else{
				fail(receive);
			}
		},()=>{
			fail(null);
		});
	}

	public void TransferCoinInGame(int toId,float amount,Action success,Action<ServerVO.TransferCoinInGameVO> fail){
		var args = new Dictionary<string,object>();
		args["a"] = "transfer_coin_in_game";
		args["uid"] = App.Instance.uid;
		args["game_id"] = App.Instance.game.game_id;
		args["token"] = App.Instance.token;
		args["dst_uid"] = toId;
		args["coin_type"] = _coin_type;
		args["amount"] = amount;
		args["time"] = App.Instance.time;
		args["sign"] = Encrypt.Md5(App.Instance.uid+""+
			App.Instance.game.game_id+""+App.Instance.token+""+toId+""+_coin_type+amount+""+args["time"]+App.key);
		Server.Instance.Get<ServerVO.TransferCoinInGameVO>(args,(receive)=>{
			if(receive.succ == 1){
				App.Instance.role_balance = receive.role_balance;
				success();
			}else{
				fail(receive);
			}
		},()=>{
			fail(null);
		});
	}

	public void BindExchangeAccid(string accid,Action success,Action<ServerVO.BindExchangeAccidVO> fail){
		var args = new Dictionary<string,object>();
		args["a"] = "bind_exchange_accid";
		args["uid"] = App.Instance.uid;
		args["game_id"] = App.Instance.game.game_id;
		args["token"] = App.Instance.token;
		args["exchange_accid"] = accid;
		args["time"] = App.Instance.time;
		args["sign"] = Encrypt.Md5(App.Instance.uid+""+App.Instance.game.game_id+""+App.Instance.token+accid+args["time"]+App.key);
		Server.Instance.Get<ServerVO.BindExchangeAccidVO>(args,(receive)=>{
			if(receive.succ == 1){
				App.Instance.exchange_accid = receive.exchange_accid;
				success();
			}else{
				fail(receive);
			}
		},()=>{
			fail(null);
		});
	}

	public void TransferCoinToExchange(float amount,Action success,Action<ServerVO.TransferCoinToExchangeVO> fail){
		var args = new Dictionary<string,object>();
		args["a"] = "transfer_coin_to_exchange";
		args["uid"] = App.Instance.uid;
		args["game_id"] = App.Instance.game.game_id;
		args["token"] = App.Instance.token;
		args["coin_type"] = _coin_type;
		args["amount"] = amount;
		args["time"] = App.Instance.time;
		args["sign"] = Encrypt.Md5(App.Instance.uid+""+App.Instance.game.game_id+""+App.Instance.token+_coin_type+amount+args["time"]+App.key);
		Server.Instance.Get<ServerVO.TransferCoinToExchangeVO>(args,(receive)=>{
			if(receive.succ == 1){
				App.Instance.role_balance = receive.role_balance;
				App.Instance.exchange_balance = receive.exchange_balance;
				success();
			}else{
				fail(receive);
			}
		},()=>{
			fail(null);
		});
	}

	public void BindWallet(string addr,Action success,Action<ServerVO.BindWalletVO> fail){
		var args = new Dictionary<string,object>();
		args["a"] = "bind_wallet";
		args["uid"] = App.Instance.uid;
		args["game_id"] = App.Instance.game.game_id;
		args["token"] = App.Instance.token;
		args["wallet_addr"] = addr;
		args["time"] = App.Instance.time;
		args["sign"] = Encrypt.Md5(App.Instance.uid+""+App.Instance.game.game_id+""+App.Instance.token+""+addr+args["time"]+App.key);
		Server.Instance.Get<ServerVO.BindWalletVO>(args,(receive)=>{
			if(receive.succ == 1){
				App.Instance.wallet_addr = receive.wallet_addr;
				success();
			}else{
				fail(receive);
			}
		},()=>{
			fail(null);
		});
	}

	public void TransferCoinToWallet(float amount,Action success,Action<ServerVO.TransferCoinToWalletVO> fail){
		var args = new Dictionary<string,object>();
		args["a"] = "transfer_coin_to_wallet";
		args["uid"] = App.Instance.uid;
		args["game_id"] = App.Instance.game.game_id;
		args["token"] = App.Instance.token;
		args["coin_type"] = _coin_type;
		args["amount"] = amount;
		args["wallet_addr"] = "";
		args["time"] = App.Instance.time;
		args["sign"] = Encrypt.Md5(App.Instance.uid+""+App.Instance.game.game_id+""+App.Instance.token+_coin_type+amount+""+args["time"]+App.key);
		Server.Instance.Get<ServerVO.TransferCoinToWalletVO>(args,(receive)=>{
			if(receive.succ == 1){
				App.Instance.role_balance = receive.role_balance;
				App.Instance.exchange_balance = receive.exchange_balance;
				success();
			}else{
				fail(receive);
			}
		},()=>{
			fail(null);
		});
	}

	public void GetCoinListToDraw(Action success,Action<ServerVO.GetCoinListToDrawVO> fail){
		var args = new Dictionary<string,object>();
		args["a"] = "get_coin_list_to_draw";
		args["uid"] = App.Instance.uid;
		args["game_id"] = App.Instance.game.game_id;
		args["token"] = App.Instance.token;
		Server.Instance.Get<ServerVO.GetCoinListToDrawVO>(args,(receive)=>{
			if(receive.succ == 1){
				App.Instance.coin_list_to_draw = receive.coin_list;
				success();
			}else{
				fail(receive);
			}
		},()=>{
			fail(null);
		});
	}

	public void DrawCoin(int CoinId, Action success,Action<ServerVO.DrawCoinVO> fail){
		var args = new Dictionary<string,object>();
		args["a"] = "draw_coin";
		args["uid"] = App.Instance.uid;
		args["game_id"] = App.Instance.game.game_id;
		args["token"] = App.Instance.token;
		args["coin_id"] = CoinId;
		args["time"] = App.Instance.time;
		args["sign"] = Encrypt.Md5(App.Instance.uid+""+
			App.Instance.game.game_id+""+App.Instance.token+CoinId+args["time"]+App.key);
		Server.Instance.Get<ServerVO.DrawCoinVO>(args,(receive)=>{
			if(receive.succ == 1){
				App.Instance.role_balance = receive.role_balance;
				success();
			}else{
				fail(receive);
			}
		},()=>{
			fail(null);
		});
	}
}
