using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using System;
public class SettingsManger : MonoBehaviour {
	public Canvas canvas;
	public Text labelBalance;
	public Text labelWallet;
	public Text labelExchange;
	public InputField labelTransferBalance;
	public InputField labelTransferId;
	public InputField inputWallet;
	public InputField inputExchange;
	public InputField inputWalletAddr;
	public InputField inputExchangeId;
	public Text tips;
	public uiManager manager;

	public void ShowSettings(){
		canvas.gameObject.SetActive(true);
	}
	public void HideSettings(){
		canvas.gameObject.SetActive(false);
		if(manager.isPause){
			manager.Resume();
		}
	}

	private void Update() {
		labelBalance.text = "BGC余额: "+App.Instance.balance.ToString();
		labelWallet.text = "钱包地址: "+App.Instance.wallet_addr;
		labelExchange.text = "交易所Id: "+App.Instance.exchange_accid;
	}

	public void TransferCoinInGame(){
		var toId = 0;
		var amount = 0f;
		try{
			toId = Convert.ToInt32(labelTransferId.text);
			amount = Convert.ToSingle(labelTransferBalance.text);
		}catch(Exception){

		}
		API.Instance.TransferCoinInGame(toId,amount,()=>{
			Debug.LogWarning("success");
			tips.text = "转给玩家成功!";
		},(e)=>{
			Debug.LogWarning("fail");
			tips.text = "转给玩家失败!";
			if(e != null){
				tips.text += e.errno;
			}
		});
	}

	public void TransferCoinToWallet(){
		var amount = 0f;
		try{
			amount = Convert.ToSingle(inputWallet.text);
		}catch(Exception){

		}
		API.Instance.TransferCoinToWallet(amount,()=>{
			Debug.LogWarning("success");
			tips.text = "转到钱包成功!";
		},(e)=>{
			Debug.LogWarning("fail");
			tips.text = "转到钱包失败!";
			if(e != null){
				tips.text += e.errno;
			}
		});
	}

	public void TransferCoinToExchange(){
		var amount = 0f;
		try{
			amount = Convert.ToSingle(inputExchange.text);
		}catch(Exception){

		}
		API.Instance.TransferCoinToExchange(amount,()=>{
			Debug.LogWarning("success");
			tips.text = "转到交易所成功!";
		},(e)=>{
			Debug.LogWarning("fail");
			tips.text = "转到交易所失败!";
			if(e != null){
				tips.text += e.errno;
			}
		});
	}

	public void BindExchangeAccid(){
		var accid = inputExchangeId.text;
		API.Instance.BindExchangeAccid(accid,()=>{
			Debug.LogWarning("success");
			tips.text = "绑定交易所成功!";
		},(e)=>{
			Debug.LogWarning("fail");
			tips.text = "绑定交易所失败!";
			if(e != null){
				tips.text += e.errno;
			}
		});
	}

	public void BindWallet(){
		var wallet = inputWalletAddr.text;
		API.Instance.BindWallet(wallet,()=>{
			Debug.LogWarning("success");
			tips.text = "绑定钱包成功！";
		},(e)=>{
			Debug.LogWarning("fail");
			tips.text = "绑定钱包失败!";
			if(e != null){
				tips.text += e.errno;
			}
		});
	}
}
