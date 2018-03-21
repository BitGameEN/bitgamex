using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Encrypt{
	public static string Md5(string data){
		var md5 = new System.Security.Cryptography.MD5CryptoServiceProvider();
		var byteValue = System.Text.Encoding.UTF8.GetBytes(data);
		var byteHash = md5.ComputeHash(byteValue);
		md5.Clear();
		var result = "";
		for(var i = 0;i<byteHash.Length;i++){
			result += byteHash[i].ToString("X").PadLeft(2,'0');
		}
		return result.ToLower();
	}
}
