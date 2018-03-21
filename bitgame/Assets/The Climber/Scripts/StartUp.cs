using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.SceneManagement;
public class StartUp : MonoBehaviour {
	public Text label;
	public Slider progress;
	// Use this for initialization
	void Start () {
		label.text = "";
		App.Instance.Init();
	}
	
	// Update is called once per frame
	void Update () {
		
	}

	public void Login(){
		label.text = "登录中。。。";
		API.Instance.LoginGame(()=>{
			label.text = "登录成功";
			label.text = "获取游戏数据中。。。";
			API.Instance.GetGame(()=>{
				label.text = "获取游戏数据成功。。。";
				SceneManager.LoadSceneAsync("GameScene",LoadSceneMode.Single);
			},()=>{
				label.text = "获取游戏数据失败";
			});
		},()=>{
			label.text = "登录失败";
		});
	}
}
